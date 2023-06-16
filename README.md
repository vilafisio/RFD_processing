**RFD Processing**

This project provides a Shiny application for processing Rate of Force Development (RFD) from force data files.

Getting Started
Clone the repository to your local machine:

git clone https://github.com/vilafisio/RFD_processing.git

Usage

This application reads force data from text files, processes the data, and allows the user to calculate and visualize RFD values. It also provides an option to download the processed results.

The application expects the data to be in specific formats:

The force data files should be .txt files, where the actual data starts after the 8th line. The files should be tab-delimited.
The application reads multiple files at a time. The order of file processing depends on the order of file selection during upload.
The file names should follow a specific pattern for proper extraction of participant, task, session, and trial information:

"<prefix>_<participant_number>_<task_number>_<session_number>_<trial_number>_<other_info>.txt"

Known Limitations

  The application is designed to process the files in sequence. When the last file is processed, the calculate button is disabled to prevent any error that could arise from trying to process a non-existing next file.

Contributing

  We welcome contributions from the community. If you'd like to contribute, please fork the repository, make your changes, and open a pull request.
