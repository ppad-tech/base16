#include <stddef.h>
#include <stdint.h>

#if defined(__aarch64__)

#include <arm_neon.h>

/* lowercase ASCII hex character for each nibble value 0..15 */
static const uint8_t hex_lut[16] = {
    '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
};

/*
 * Encode 'l' input bytes at 'src' into 2*l ASCII hex bytes at 'dst'.
 *
 * NEON kernel processes 16 input bytes per iteration:
 *   - vshrq_n_u8 / vandq_u8 split the high and low nibbles
 *   - vqtbl1q_u8 looks up each nibble in 'hex_lut' (the tbl instr)
 *   - vst2q_u8 stores the two char vectors interleaved, producing
 *     [h0 l0 h1 l1 ... h15 l15] which is exactly the hex output
 *
 * A scalar tail finishes the final (l mod 16) bytes.
 */
void base16_encode_arm(const uint8_t *src, uint8_t *dst, size_t l) {
    uint8x16_t lut     = vld1q_u8(hex_lut);
    uint8x16_t mask_lo = vdupq_n_u8(0x0f);
    size_t i = 0;

    for (; i + 16 <= l; i += 16) {
        uint8x16_t in     = vld1q_u8(src + i);
        uint8x16_t hi     = vshrq_n_u8(in, 4);
        uint8x16_t lo     = vandq_u8(in, mask_lo);
        uint8x16x2_t pair = { { vqtbl1q_u8(lut, hi),
                                vqtbl1q_u8(lut, lo) } };
        vst2q_u8(dst + 2 * i, pair);
    }

    for (; i < l; i++) {
        uint8_t b = src[i];
        dst[2 * i]     = hex_lut[b >> 4];
        dst[2 * i + 1] = hex_lut[b & 0x0f];
    }
}

/*
 * Convert 16 ASCII hex chars to nibble values (0..15) in 'nib'.
 * Each lane of 'bad' is set to 0xff if the corresponding input is
 * not a valid hex digit ('0'..'9', 'a'..'f', 'A'..'F'), 0x00 if it
 * is.  Case-insensitive.
 */
static inline void ascii_to_nibble(uint8x16_t c,
                                   uint8x16_t *nib,
                                   uint8x16_t *bad) {
    uint8x16_t is_digit = vandq_u8(vcgeq_u8(c, vdupq_n_u8('0')),
                                    vcleq_u8(c, vdupq_n_u8('9')));
    uint8x16_t is_lower = vandq_u8(vcgeq_u8(c, vdupq_n_u8('a')),
                                    vcleq_u8(c, vdupq_n_u8('f')));
    uint8x16_t is_upper = vandq_u8(vcgeq_u8(c, vdupq_n_u8('A')),
                                    vcleq_u8(c, vdupq_n_u8('F')));

    /* offset to subtract from c: '0' (0x30), 'a'-10 (0x57), 'A'-10
     * (0x37); zero in lanes that aren't valid hex (the resulting
     * nibble in those lanes is garbage but 'bad' flags them).      */
    uint8x16_t off = vorrq_u8(
        vandq_u8(is_digit, vdupq_n_u8(0x30)),
        vorrq_u8(
            vandq_u8(is_lower, vdupq_n_u8(0x57)),
            vandq_u8(is_upper, vdupq_n_u8(0x37))));

    *nib = vsubq_u8(c, off);
    *bad = vmvnq_u8(vorrq_u8(is_digit, vorrq_u8(is_lower, is_upper)));
}

static inline uint8_t scalar_nib(uint8_t c) {
    if (c >= '0' && c <= '9') return (uint8_t)(c - '0');
    if (c >= 'a' && c <= 'f') return (uint8_t)(c - 'a' + 10);
    if (c >= 'A' && c <= 'F') return (uint8_t)(c - 'A' + 10);
    return 0x80;  /* invalid sentinel */
}

/*
 * Decode 2*outlen hex chars at 'src' into 'outlen' bytes at 'dst'.
 * Returns 1 on success, 0 if any invalid hex char was seen (in which
 * case the contents of 'dst' are unspecified).
 *
 * NEON kernel processes 32 input chars per iteration:
 *   - vld2q_u8 deinterleaves into a vector of high-nibble chars and a
 *     vector of low-nibble chars
 *   - ascii_to_nibble validates and converts each char to its nibble
 *   - vshlq_n_u8 + vorrq_u8 packs nibble pairs into output bytes
 *   - the per-iteration 'bad' masks are OR-accumulated; we reduce
 *     once at the end with vmaxvq_u8
 *
 * A scalar tail finishes the final (outlen mod 16) output bytes.
 */
int base16_decode_arm(const uint8_t *src, uint8_t *dst, size_t outlen) {
    uint8x16_t bad = vdupq_n_u8(0);
    size_t i = 0;

    for (; i + 16 <= outlen; i += 16) {
        uint8x16x2_t pair = vld2q_u8(src + 2 * i);
        uint8x16_t nib_hi, nib_lo, bad_hi, bad_lo;
        ascii_to_nibble(pair.val[0], &nib_hi, &bad_hi);
        ascii_to_nibble(pair.val[1], &nib_lo, &bad_lo);
        uint8x16_t byte = vorrq_u8(vshlq_n_u8(nib_hi, 4), nib_lo);
        vst1q_u8(dst + i, byte);
        bad = vorrq_u8(bad, vorrq_u8(bad_hi, bad_lo));
    }

    uint8_t tail_bad = 0;
    for (; i < outlen; i++) {
        uint8_t n0 = scalar_nib(src[2 * i]);
        uint8_t n1 = scalar_nib(src[2 * i + 1]);
        tail_bad |= (n0 | n1) & 0x80;
        dst[i] = (uint8_t)((n0 << 4) | (n1 & 0x0f));
    }

    return (vmaxvq_u8(bad) == 0) && (tail_bad == 0);
}

int base16_arm_available(void) {
    return 1;
}

#else

/* stubs for non-aarch64 builds; never reached because dispatch is
 * gated on 'base16_arm_available' returning 0                     */

void base16_encode_arm(const uint8_t *src, uint8_t *dst, size_t l) {
    (void)src; (void)dst; (void)l;
}

int base16_decode_arm(const uint8_t *src, uint8_t *dst, size_t outlen) {
    (void)src; (void)dst; (void)outlen;
    return 0;
}

int base16_arm_available(void) {
    return 0;
}

#endif
