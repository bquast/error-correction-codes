# Setup parameters for Reed-Solomon
n <- 7  # Length of codeword (n = 2^m - 1 for m=3 gives n=7)
k <- 3  # Number of data symbols
q <- 8  # Size of Galois Field GF(2^3)

# Galois Field GF(2^3)
gf <- c(1, 2, 4, 3, 6, 7, 5)  # Elements of GF(2^3)
gf_inv <- c(1, 1, 5, 2, 4, 7, 6)  # Inverse mapping for GF(2^3)

# Message to encode
message <- c(1, 2, 3)

# Initialize codeword with message
codeword <- c(message, rep(0, n - k))

# Compute parity symbols
for (i in (k+1):n) {
  for (j in 1:k) {
    codeword[i] <- (codeword[i] + message[j] * gf[(i-j) %% 7 + 1]) %% q  # Mod q for GF(2^3)
  }
}

# Print the encoded codeword
cat("Encoded Codeword: ", codeword, "\n")

# Introduce a symbol-level error (for example, add 2 to a symbol)
codeword_with_error <- codeword
codeword_with_error[2] <- (codeword_with_error[2] + 2) %% q  # Introduce a symbol error

# Print the codeword with an error
cat("Codeword with Error: ", codeword_with_error, "\n")

# Calculate syndromes
syndrome <- rep(0, n - k)
for (i in 1:(n - k)) {
  for (j in 1:n) {
    syndrome[i] <- (syndrome[i] + codeword_with_error[j] * gf[((j-1)*i) %% 7 + 1]) %% q
  }
}

# Print the calculated syndromes
cat("Syndromes: ", syndrome, "\n")

# Check if there is any error by examining the syndromes
if (all(syndrome == 0)) {
  cat("No error detected.\n")
} else {
  # Try to locate and correct the error
  for (i in 1:n) {
    syndrome_sum <- 0
    for (j in 1:(n - k)) {
      syndrome_sum <- (syndrome_sum + syndrome[j] * gf_inv[((i-1)*j) %% 7 + 1]) %% q
    }
    
    if (syndrome_sum == 0) {
      # Error location found at position i
      codeword_with_error[i] <- (codeword_with_error[i] - 2) %% q  # Correct the error
      break
    }
  }
}

# Print the corrected codeword
cat("Corrected Codeword: ", codeword_with_error, "\n")

# Extract the original message from the corrected codeword
decoded_message <- codeword_with_error[1:k]

# Print the original message and the decoded message
cat("Original Message: ", message, "\n")
cat("Decoded Message: ", decoded_message, "\n")