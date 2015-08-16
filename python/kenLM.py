import sys
import math
import kenlm
import json

def count_probabilities(first_ngram, second_ngram, candidates):
    model = kenlm.LanguageModel('data/text.binary')
    result = {}
    for candidate in candidates:
        first_ngram_prob = math.exp(model.score(part1 + " " + candidate))
        second_ngram_prob = math.exp(model.score(candidate + " " + part2))
        result[candidate] = first_ngram_prob + second_ngram_prob
    return result

part1 = " ".join(sys.argv[1:3])
part2 = " ".join(sys.argv[3:5])
candidates = sys.argv[5:]

result = count_probabilities(part1, part2, candidates)

json.dump(result, sys.stdout)
