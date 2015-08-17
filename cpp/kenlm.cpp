#include "noisy_text_kenlm.h"

#define KENLM_MAX_ORDER 6

#include "lm/model.hh"
#include <string>
#include <cstdlib>

using namespace lm::ngram;

Model model("data/text.binary");
const Vocabulary &vocab = model.GetVocabulary();

static inline
double sentence_probability(const State& state, const char * const words[], int len, State &out_state) {
	State cur_state = state;
	double last_score = 0;
	for (int i = 0; i < len; ++i) {
		last_score = model.Score(cur_state, vocab.Index(words[i]), out_state);
		cur_state = out_state;
	}
	return last_score;
}

static inline
void part1_probabilities(const char * const part1[], int part1_len, const char * const candidates[], int candidates_len, double results[]) {
	State state(model.BeginSentenceState());
	State out_state;

	for (int i = 0; i < part1_len; ++i) {
		model.Score(state, vocab.Index(part1[i]), out_state);
		state = out_state;
	}

	for (int i = 0; i < candidates_len; ++i) {
		results[i] = model.Score(state, vocab.Index(candidates[i]), out_state);
		/* Don't update state now */
	}
}

extern "C"
double get_sentence_probability(const char * const sentence[], int len) {
	State state(model.BeginSentenceState());
	State out_state;
	return std::exp(sentence_probability(state, sentence, len, out_state));
}

extern "C"
void language_model(
		const char * const part1[], int part1_len,
		const char * const part2[], int part2_len,
		const char * const cands[], int cands_len,
		double results[]) {
	part1_probabilities(part1, part1_len, cands, cands_len, results);

	const char **sentence = (const char **)std::malloc(sizeof(char *) * (part2_len + 1));
	memcpy(sentence + 1, part2, sizeof(*part2) * part2_len);
	for (int i = 0; i < cands_len; ++i) {
		sentence[0] = cands[i];
		State state(model.NullContextState());
		State out_state;
		results[i] += sentence_probability(state, sentence, part2_len + 1, out_state);
		results[i] = std::exp(results[i]);
	}
}
