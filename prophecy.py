#!/usr/bin/env python3
import csv
import sys

from cs50 import SQL

def main():

    # Ensure correct usage
    if len(sys.argv) != 2:
        sys.exit("Incorrect Usage: python prophecy.py [FILENAME.csv]")

    db = SQL("sqlite:///roster.db")
    # Delete contents of assignments first because of foreign key constraints
    db.execute("DELETE FROM assignments")
    db.execute("DELETE FROM houses")
    db.execute("DELETE FROM students")

    houses = []
    with open(sys.argv[1], "r") as file:
        reader = csv.DictReader(file)

        houses = sorted(list({(row["house"], row["head"]) for row in reader}))
        # Reset file pointer to the beginning of the file
        file.seek(0)
        next(reader)  # Skip the header row

        for row in reader:
            db.execute("BEGIN TRANSACTION")
            db.execute("INSERT INTO students (id, student_name) VALUES (?,?);", row["id"], row["student_name"])
            db.execute("COMMIT")
            for i, house in enumerate(houses):
                if house[0] == row["house"]:
                    try:
                          db.execute("INSERT INTO houses (id, house, head) VALUES (?,?,?)", i+1, house[0], house[1])
                    except:
                        continue
            for i, house in enumerate(houses):
                if house[0] == row["house"]:
                    db.execute("INSERT INTO assignments (student_id, house_id) VALUES (?,?);", row["id"], i+1)


if __name__ == "__main__":
    main()

