def main():
    CON_DIGIT = 12
    ans = 0

    with open("INPUT", "r") as f:
        for line in f:
            line = line.rstrip("\n")
            pos = 0
            num_chars = []

            for i in range(CON_DIGIT):
                max_chr = '0'
                limit = 100 - (CON_DIGIT - (i + 1))

                for j in range(pos, limit):
                    if line[j] > max_chr:
                        max_chr = line[j]
                        pos = j

                num_chars.append(max_chr)
                pos += 1
            ans += int("".join(num_chars))

    print(ans)

if __name__ == "__main__":
    main()
