# Initialize the message to be encoded and corrected
message <- c(1, 0, 1, 1)  # Original message (4 bits)
cat("Original Message:", message, "\n")

# Step 1: Generate Hamming Code (7,4) for the message

# The message has 4 bits, and we'll calculate 3 parity bits

# Initialize the Hamming code array with 7 positions
hamming_code <- integer(7)

# Assign the message bits to their positions
hamming_code[3] <- message[1]  # m1
hamming_code[5] <- message[2]  # m2
hamming_code[6] <- message[3]  # m3
hamming_code[7] <- message[4]  # m4

# Calculate parity bits
hamming_code[1] <- hamming_code[3] + hamming_code[5] + hamming_code[7]  # p1
hamming_code[2] <- hamming_code[3] + hamming_code[6] + hamming_code[7]  # p2
hamming_code[4] <- hamming_code[5] + hamming_code[6] + hamming_code[7]  # p3

# Reduce modulo 2 to ensure bits are either 0 or 1
hamming_code <- hamming_code %% 2

# The encoded Hamming code
cat("Encoded Hamming Code:", hamming_code, "\n")

# Step 2: Simulate an error (let's flip the 5th bit)
hamming_code[5] <- 1 - hamming_code[5]
cat("Hamming Code with Error:", hamming_code, "\n")

# Step 3: Detect and correct the error

# Calculate the syndrome
syndrome <- integer(3)
syndrome[1] <- hamming_code[1] + hamming_code[3] + hamming_code[5] + hamming_code[7]  # s1
syndrome[2] <- hamming_code[2] + hamming_code[3] + hamming_code[6] + hamming_code[7]  # s2
syndrome[3] <- hamming_code[4] + hamming_code[5] + hamming_code[6] + hamming_code[7]  # s3

# Reduce modulo 2
syndrome <- syndrome %% 2

# Calculate the position of the error
error_position <- syndrome[1] * 1 + syndrome[2] * 2 + syndrome[3] * 4

# If error_position is not 0, flip the bit at that position
if (error_position != 0) {
  hamming_code[error_position] <- 1 - hamming_code[error_position]
  cat("Error detected at position:", error_position, "\n")
}

# The corrected Hamming code
cat("Corrected Hamming Code:", hamming_code, "\n")

# Step 4: Extract the original message from the corrected Hamming code
corrected_message <- c(hamming_code[3], hamming_code[5], hamming_code[6], hamming_code[7])
cat("Corrected Message:", corrected_message, "\n")