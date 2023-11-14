#!/usr/bin/env python3
def main():
    value = input("Number: ")

    try:
        value = int(value)
    except ValueError:
        return 1

    if value <= 0:
        return 1

    value = str(value)

    if isLuhn(value):
        if len(value) == 15 and value[:2] in ("34", "37"):
            print("AMEX\n")
        elif len(value) == 16 and value[:2] in ("51", "52", "53", "54", "55"):
            print("MASTERCARD\n")
        elif len(value) in (13, 16) and value[0] == "4":
            print("VISA\n")
        else:
            print("INVALID\n")
    else:
        print("INVALID\n")


def isLuhn(value):
    '''Implements Luhn's Algo'''
    digits = [int(digit) for digit in value]
    digit_sum = 0

    tmp = [digit * 2 for digit in digits[-2::-2]]
    digit_sum += sum([int(digit) for number in tmp for digit in str(number)])
    digit_sum += sum([digit for digit in digits[-1::-2]])

    if digit_sum % 10 == 0:
        return True
    else:
        return False


if __name__ == "__main__":
    main()

