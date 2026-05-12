# ============================================================
# Random Forest Classifier for Animal Calls
# Uses MFCC acoustic features extracted from .wav files
# ============================================================

# --- Load required packages ---
library(gibbonR)      # MFCC feature extraction from audio
library(randomForest) # Random Forest model
library(caret)        # Confusion matrix evaluation
library(tidyverse)    # Data wrangling

# ============================================================
# WHAT ARE MFCCs?
# ============================================================
#
# MFCC stands for Mel-Frequency Cepstral Coefficients.
#
# 1. SLICING THE CALL
#    We take a short call recording and slice it into small
#    time windows (n.windows = 8 means 8 slices per call).
#    This captures how the sound changes over time.
#
# 2. DESCRIBING EACH SLICE
#    For each slice, we ask: which frequencies are loud,
#    and which are quiet? This gives us the "spectral shape"
#    of that moment in the call — like a snapshot of the
#    sound's texture at that instant.
#
# 3. COMPRESSING THE DESCRIPTION
#    A full frequency spectrum has thousands of values.
#    MFCCs compress this into just a few numbers (num.cep = 12)
#    that capture the most important spectral shape information.
#
# 4. THE RESULT
#    Each call becomes a row of 8 × 12 = 96 numbers.
#    These numbers don't mean anything to us directly —
#    we can't look at "MFCC coefficient 7 = -1.3" and say
#    "ah, that's a Drongo". But calls from the same species
#    tend to produce similar MFCC patterns, and that's
#    exactly what the Random Forest learns to recognize.
#

# ============================================================
# WHAT IS A RANDOM FOREST?
# ============================================================
#
# A Random Forest is a machine learning method that learns to
# recognize patterns in data and use them to classify new examples.
#
# In our case: we want the model to listen to an animal call
# and decide — which species or call type is this?
#
# --- How it works, step by step: ---
#
# 1. FEATURES
#    We don't feed raw audio into the model. Instead, we first
#    summarize each call as a row of numbers (MFCC features) —
#    think of them as an acoustic "fingerprint" of the sound.
#
# 2. DECISION TREES
#    Imagine you're trying to identify a call by asking simple questions:
#
#      "Is this call high-pitched?"
#           YES → "Is it longer than 1 second?"
#                      YES → probably a Drongo
#                      NO  → probably a Dove
#           NO  → probably a Sparrow
#
#    A decision tree learns these questions automatically from your data.
#    It figures out which MFCC features best separate the call types, and 
#    builds a flowchart of splits.
#    At the end of the flowchart, it gives a predicted class label.
#
# 3. THE FOREST
#    One tree is fragile — small changes in data can flip its answers.
#    A Random Forest builds hundreds of trees (ntree = 1000 here),
#    each trained on a random subset of the data and features.
#    Final prediction = majority vote across all trees.
#    More trees → more stable and reliable predictions.
#
# 4. RANDOMNESS (the "Random" in Random Forest)
#    At each split in each tree, only a random subset of features
#    (mtry) is considered. This forces trees to be different from
#    each other, which makes the combined prediction more robust.
#
# 5. OUT-OF-BAG (OOB) ERROR
#    Each tree is trained on ~63% of the data. The remaining ~37%
#    (the "out-of-bag" samples) are used to test that tree.
#    This gives a free accuracy estimate without needing a
#    separate validation set.
#
# --- Key parameters in this script: ---
#
#   ntree      = number of trees in the forest (more = better, but slower)
#   mtry       = features considered at each split (we tune this automatically)
#   importance = whether to track which features were most useful
#
# --- Rule of thumb for interpreting results: ---
#
#   Training accuracy ≈ 100%  → normal, the model has "seen" this data
#   Test accuracy             → what actually matters! Measures generalization
#   OOB error                 → a reliable estimate even before the test set
#
# ============================================================

# --- Set path to folder with labeled .wav files ---
# Files must be named: ClassName_anything.wav (e.g. "Hyena_call01.wav")
TrainingDataDirectory <- "C:/Users/vdemartsev/ownCloud/SA_field_course/Program/CBEHAV_KRC_Scripts/PAM/test_data/cut_calls/"

# ============================================================
# STEP 1: Extract MFCC Features from Audio Files
# ============================================================
# MFCCs (Mel-Frequency Cepstral Coefficients) describe the
# spectral shape of a sound — they are commonly used as 
# acoustic "fingerprints" for classifying animal calls.

trainingdata <- gibbonR::MFCCFunction(
  input.dir    = TrainingDataDirectory,
  min.freq     = 50,       # Minimum frequency to include (Hz)
  max.freq     = 6000,     # Maximum frequency to include (Hz)
  n.windows    = 8,        # Number of time windows per call
  num.cep      = 12,       # Number of MFCC coefficients per window
  win.avg      = "standard",
  win.hop.time = 0.01
)

# ============================================================
# STEP 2: Clean the Data
# ============================================================

# The first column ("class") contains the call type labels.
# Convert it to a factor (categorical variable) — required by randomForest.
trainingdata$class <- as.factor(trainingdata$class)

# Remove any rows with missing values (can occur for very short files)
trainingdata <- trainingdata[complete.cases(trainingdata), ]

# Fix column names: R doesn't like column names that start with numbers
# (e.g. "1", "2" → "X1", "X2")
names(trainingdata) <- make.names(names(trainingdata))

# Check how many examples we have per class
table(trainingdata$class)

# ============================================================
# STEP 3: Split into Training (40%) and Test (60%) Sets
# ============================================================
# We train the model on one subset and evaluate it on the other.
# The test set simulates "new, unseen" data.

set.seed(3) # Makes the random split reproducible

testindex        <- sample(1:nrow(trainingdata), round(nrow(trainingdata) * 0.6), replace = FALSE)
testdata         <- trainingdata[testindex, ]   # 60% held out for testing
trainingdata_sub <- trainingdata[-testindex, ]  # 40% used for training

# ============================================================
# STEP 4: Tune the Random Forest (Find Optimal mtry)
# ============================================================
# Random Forest builds many decision trees and combines their votes.
# "mtry" = number of features randomly considered at each tree split.
# Too few → trees are too random; too many → trees are too similar.
# We test different mtry values and pick the one with lowest OOB error.
#
# OOB (Out-Of-Bag) error: an internal accuracy estimate using the
# data not used to build each tree — no separate validation set needed.

oob_errors <- sapply(seq(2, ncol(trainingdata_sub) - 1, by = 2), function(m) {
  rf <- randomForest(
    x    = trainingdata_sub[, 2:ncol(trainingdata_sub)], # MFCC features
    y    = trainingdata_sub$class,                        # Call labels
    mtry = m, ntree = 300
  )
  rf$err.rate[nrow(rf$err.rate), "OOB"] # Return final OOB error
})

optimal_mtry <- seq(2, ncol(trainingdata_sub) - 1, by = 2)[which.min(oob_errors)]
message(paste("Optimal mtry found:", optimal_mtry))

# ============================================================
# STEP 5: Train the Final Random Forest Model
# ============================================================
# Now we train a full model using the best mtry and more trees (ntree).
# More trees = more stable predictions, but slower to compute.

ml.model.rf <- randomForest(
  x          = trainingdata_sub[, 2:ncol(trainingdata_sub)],
  y          = trainingdata_sub$class,
  mtry       = optimal_mtry,
  ntree      = 1000,
  importance = TRUE  # Allows us to inspect which features matter most
)

# ============================================================
# STEP 6: Evaluate the Model
# ============================================================

# --- Check fit on TRAINING data (should be near-perfect — not very meaningful) ---
rf.predict.train <- predict(ml.model.rf, trainingdata_sub[, -1])
caret::confusionMatrix(rf.predict.train, trainingdata_sub$class)

# --- Evaluate on TEST data (this is what really matters!) ---
rf.predict.test <- predict(ml.model.rf, testdata[, -1])
cm <- caret::confusionMatrix(rf.predict.test, testdata$class)
print(cm)

# ============================================================
# STEP 7: Plot Confusion Matrix as Heatmap (Recall %)
# ============================================================
# Recall = of all true calls of a class, how many did we correctly detect?
# This matters more than raw counts when class sizes differ.

cm_table <- as.data.frame(cm$table) %>%
  group_by(Reference) %>%
  mutate(
    Freq_Percent         = Freq / sum(Freq) * 100,
    Freq_Percent_Rounded = round(Freq_Percent, 1)
  ) %>%
  ungroup()

ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq_Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq_Percent_Rounded), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue",
                      name = "Recall (%)") +
  labs(title = "Confusion Matrix — Recall per Class",
       x = "Actual Class", y = "Predicted Class") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

