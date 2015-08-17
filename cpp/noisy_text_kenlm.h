#ifndef NOISY_TEXT_KENLM_H
#define NOISY_TEXT_KENLM_H

#ifdef __cplusplus
extern "C" {
#endif

double get_sentence_probability(const char * const sentence[], int len);

void language_model(
		const char * const part1[], int part1_len,
		const char * const part2[], int part2_len,
		const char * const cands[], int cands_len,
		double results[]);

#ifdef __cplusplus
}
#endif

#endif /* NOISY_TEXT_KENLM_H */
