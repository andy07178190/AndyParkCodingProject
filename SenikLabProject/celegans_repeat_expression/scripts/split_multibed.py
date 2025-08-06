input_file = "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_elements.bed"
output_file = "all_repeat_elements.cleaned.bed"

with open(input_file) as f_in, open(output_file, "w") as f_out:
    for line in f_in:
        parts = line.strip().split("\t")
        if len(parts) < 5:
            continue  # skip malformed lines
        name = parts[0]
        chrs = parts[1].split(";")
        starts = parts[2].split(";")
        ends = parts[3].split(";")

        for chr_, start, end in zip(chrs, starts, ends):
            f_out.write(f"{chr_}\t{start}\t{end}\t{name}\n")
