
# ZUD - Transport Management Tool

This ABAP tool facilitates managing SAP transport requests, allowing users to upload, download, and import transport files with ease. 

## Features

- **Transport Request Management**:
  - Upload and Download transport requests.
  - Import transport requests into the system.

- **Error Handling**:
  - Clear and concise error messages for missing or incorrect inputs, such as:
    - *Request number cannot be empty* (if the user doesn't provide a request number).
    - *Folder cannot be empty* (if no folder is selected for download/upload).
    - *File name cannot be empty* (if the user does not select a file for upload).
  - Outputs success messages on successful operations:
    - *Download was successful*.
    - *Upload was successful*.

## Getting Started

To get started with the **Transport Management Tool**, follow the steps below:

1. **Clone or Download**:
   - Clone this repository or download the source code to your local machine.
   
2. **Import into SAP**:
   - Import the ABAP source code into your SAP system.

3. **Configure Authorizations**:
   - Ensure that the required authorizations are in place to allow access to transport requests and file systems.

4. **Start Using**:
   - Once the tool is set up, use the selection screen to specify the parameters and select the appropriate action (upload/download).

## Usage

1. **Selection Screen**:  
   Use the selection screen to specify the required parameters:  
   - **Request Number**: Enter or select a transport request.
   - **Folder Path**: Specify the target folder for download
   - **File Path**: Specify the file for uploading transport requests.

2. **Download a Transport Request**:  
   - Select the `Download` option and specify the `Request Number` and `Folder Path`.
   - The tool downloads the transport request to the specified folder.

3. **Upload a Transport Request**:  
   - Select the `Upload` option and specify the `File Path`.
   - Optionally, choose `Import` to directly import the uploaded transport into the system.

## Example Scenarios

- **Scenario 1: Download a Transport Request**
  1. Open the selection screen.
  2. Enter the transport request number (e.g., `SAPK900001`).
  3. Select the target folder for downloading.
  4. Click "Download" to save the transport request to the folder.

- **Scenario 2: Upload a Transport Request**
  1. Open the selection screen.
  2. Select the file to upload (e.g., `SAPK900001.zip`).
  3. Optionally, check "Import" to import the transport request directly.
  4. Click "Upload" to start the upload process.

## Requirements

- SAP GUI
- Authorization for transport management
- File system access for upload/download

## License

This project is licensed under the MIT License.
