# Initialize the 4x4 matrix with data bits
# The first 9 positions are data bits, and the remaining 7 are parity bits.
matrix_4x4 <- matrix(integer(16), nrow = 4, ncol = 4)

# Assigning the message bits (9 bits)
message <- c(1, 0, 1, 
             1, 1, 0, 
             0, 1, 1)

matrix_4x4[1, 2] <- message[1]  # m1
matrix_4x4[1, 3] <- message[2]  # m2
matrix_4x4[1, 4] <- message[3]  # m3
matrix_4x4[2, 2] <- message[4]  # m4
matrix_4x4[2, 3] <- message[5]  # m5
matrix_4x4[2, 4] <- message[6]  # m6
matrix_4x4[3, 2] <- message[7]  # m7
matrix_4x4[3, 3] <- message[8]  # m8
matrix_4x4[3, 4] <- message[9]  # m9

# Calculate the parity bits
# p1 covers m1, m2, m3
matrix_4x4[1, 1] <- (matrix_4x4[1, 2] + matrix_4x4[1, 3] + matrix_4x4[1, 4]) %% 2

# p2 covers m1, m4, m7
matrix_4x4[2, 1] <- (matrix_4x4[1, 2] + matrix_4x4[2, 2] + matrix_4x4[3, 2]) %% 2

# p3 covers m2, m5, m8
matrix_4x4[3, 1] <- (matrix_4x4[1, 3] + matrix_4x4[2, 3] + matrix_4x4[3, 3]) %% 2

# p4 covers m3, m6, m9
matrix_4x4[4, 1] <- (matrix_4x4[1, 4] + matrix_4x4[2, 4] + matrix_4x4[3, 4]) %% 2

# p5 covers m1, m5, m9
matrix_4x4[4, 2] <- (matrix_4x4[1, 2] + matrix_4x4[2, 3] + matrix_4x4[3, 4]) %% 2

# p6 covers m3, m5, m7
matrix_4x4[4, 3] <- (matrix_4x4[1, 4] + matrix_4x4[2, 3] + matrix_4x4[3, 2]) %% 2

# p7 covers m1, m6, m8
matrix_4x4[4, 4] <- (matrix_4x4[1, 2] + matrix_4x4[2, 4] + matrix_4x4[3, 3]) %% 2

# The encoded 4x4 matrix
cat("Encoded 4x4 Matrix:\n")
print(matrix_4x4)

# Step 2: Simulate an error (let's flip the 6th bit, which is m6)
matrix_4x4[2, 4] <- 1 - matrix_4x4[2, 4]
cat("4x4 Matrix with Error:\n")
print(matrix_4x4)

# Step 3: Detect and correct the error

# Calculate the syndromes for each parity bit
syndrome <- integer(7)

# Syndrome calculation based on parity bits
syndrome[1] <- (matrix_4x4[1, 1] + matrix_4x4[1, 2] + matrix_4x4[1, 3] + matrix_4x4[1, 4]) %% 2
syndrome[2] <- (matrix_4x4[2, 1] + matrix_4x4[1, 2] + matrix_4x4[2, 2] + matrix_4x4[3, 2]) %% 2
syndrome[3] <- (matrix_4x4[3, 1] + matrix_4x4[1, 3] + matrix_4x4[2, 3] + matrix_4x4[3, 3]) %% 2
syndrome[4] <- (matrix_4x4[4, 1] + matrix_4x4[1, 4] + matrix_4x4[2, 4] + matrix_4x4[3, 4]) %% 2
syndrome[5] <- (matrix_4x4[4, 2] + matrix_4x4[1, 2] + matrix_4x4[2, 3] + matrix_4x4[3, 4]) %% 2
syndrome[6] <- (matrix_4x4[4, 3] + matrix_4x4[1, 4] + matrix_4x4[2, 3] + matrix_4x4[3, 2]) %% 2
syndrome[7] <- (matrix_4x4[4, 4] + matrix_4x4[1, 2] + matrix_4x4[2, 4] + matrix_4x4[3, 3]) %% 2

# Calculate the error position using the syndromes
error_position <- syndrome[1] * 1 + syndrome[2] * 2 + syndrome[3] * 3 + syndrome[4] * 4 +
  syndrome[5] * 5 + syndrome[6] * 6 + syndrome[7] * 7

# Determine the row and column for the error
row_error <- (error_position - 1) %/% 4 + 1
col_error <- (error_position - 1) %% 4 + 1

# If error_position is not 0, flip the bit at that position
if (error_position != 0) {
  matrix_4x4[row_error, col_error] <- 1 - matrix_4x4[row_error, col_error]
  cat("Error detected at position: (Row:", row_error, ", Column:", col_error, ")\n")
}

# The corrected 4x4 matrix
cat("Corrected 4x4 Matrix:\n")
print(matrix_4x4)

# Step 4: Extract the original message from the corrected 4x4 matrix
corrected_message <- c(matrix_4x4[1, 2], matrix_4x4[1, 3], matrix_4x4[1, 4],
                       matrix_4x4[2, 2], matrix_4x4[2, 3], matrix_4x4[2, 4],
                       matrix_4x4[3, 2], matrix_4x4[3, 3], matrix_4x4[3, 4])

cat("Corrected Message:", corrected_message, "\n")
