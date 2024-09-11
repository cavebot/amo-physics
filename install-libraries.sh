#!/bin/bash

# Function to determine the OS type
get_os_type() {
    echo "$OSTYPE"
}

# Function to install NetCDF based on the operating system
install_netcdf() {
    local os_type=$1

    if [[ "$os_type" == "darwin"* ]]; then
        # Check if MacPorts is installed
        if ! command -v port &> /dev/null; then
            echo "MacPorts is not installed. Please install MacPorts first."
            echo "Visit https://www.macports.org/install.php for instructions."
            exit 1
        fi

        # Install NetCDF-Fortran using MacPorts
        echo "Installing NetCDF-Fortran using MacPorts..."
        sudo port install netcdf-fortran

        if [ $? -eq 0 ]; then
            echo "NetCDF-Fortran installed successfully on macOS."
        else
            echo "Failed to install NetCDF-Fortran on macOS."
            exit 1
        fi
    elif [[ "$os_type" == "linux-gnu"* ]]; then
        # Update package list
        echo "Updating package list..."
        sudo apt-get update

        # Install NetCDF-Fortran using apt-get
        echo "Installing NetCDF-Fortran using apt-get..."
        sudo apt-get install -y libnetcdff-dev

        if [ $? -eq 0 ]; then
            echo "NetCDF-Fortran installed successfully on Linux."
        else
            echo "Failed to install NetCDF-Fortran on Linux."
            exit 1
        fi
    else
        echo "Unsupported operating system: $os_type"
        exit 1
    fi
}

# Function to install LAPACK based on the operating system
install_lapack_and_openblas() {
    local os_type=$1

    if [[ "$os_type" == "darwin"* ]]; then
        # Check if Homebrew is installed
        if ! command -v brew &> /dev/null; then
            echo "Homebrew is not installed. Please install Homebrew first."
            echo "Visit https://brew.sh/ for instructions."
            exit 1
        fi

        # Install LAPACK using Homebrew
        echo "Installing LAPACK using Homebrew..."
        brew install lapack

        if [ $? -eq 0 ]; then
            echo "LAPACK installed successfully on macOS."
        else
            echo "Failed to install LAPACK on macOS."
            exit 1
        fi

        brew install openblas
        if [ $? -eq 0 ]; then
            echo "OpenBLAS installed successfully on macOS."
        else
            echo "Failed to install OpenBLAS on macOS."
            exit 1
        fi

    elif [[ "$os_type" == "linux-gnu"* ]]; then
        # Update package list
        echo "Updating package list..."
        sudo apt-get update

        # Install LAPACK using apt-get
        echo "Installing LAPACK using apt-get..."
        sudo apt-get install -y liblapack-dev

        if [ $? -eq 0 ]; then
            echo "LAPACK installed successfully on Linux."
        else
            echo "Failed to install LAPACK on Linux."
            exit 1
        fi
    
            # Verify this copilot suggestion

        # Install OpenBLAS using apt-get
        # sudo apt-get install -y libopenblas-dev
        # if [ $? -eq 0 ]; then
        #     echo "OpenBLAS installed successfully on Linux."
        # else
        #     echo "Failed to install OpenBLAS on Linux."
        #     exit 1
        # fi


    else
        echo "Unsupported operating system: $os_type"
        exit 1
    fi
}

# Function to compile the Slatec library from source
compile_slatec() {
    # Determine the OS type for conditional ar command
    local os_type=$1

    if [[ "$os_type" == "darwin"* ]]; then
        # Create the library archive on macOS using macOS-compatible ar commands
        echo "Installing Slatec library on macOS with macPorts..."
        sudo port install slatec
    elif [[ "$os_type" == "linux-gnu"* ]]; then

        local src_dir="./lib"
        local tar_file="$src_dir/slatec.tar.gz"
        local build_dir="$src_dir/slatec/src"

        # Extract the source archive
        echo "Extracting Slatec source files..."
        tar -xzf "$tar_file" -C "$src_dir"

        # Change to the extracted directory
        cd "$build_dir" || { echo "Failed to change directory to $build_dir"; exit 1; }

        # Compile the source files
        echo "Compiling Slatec source files into a static library..."
        gfortran -O2 -c *.f
        ar cst libslatec.a *.o
        # Move the library archive to the lib directory
        mv libslatec.a ../..
        # Return to the original directory
        cd ../.. || { echo "Failed to return to the original directory"; exit 1; }
        rm -r slatec

        echo "Slatec library compiled and moved to $src_dir"
    fi

   
}

# Function to compile the Nag17 library from source
compile_nag17() {
    local lib_dir="./lib"
    local tar_file="$lib_dir/nag17.tar.gz"
    local build_dir="$lib_dir/nag17/scripts"

    # Extract the source archive
    echo "Extracting NAG17 source files..."
    tar -xzf "$tar_file" -C "$lib_dir"

    # Run the script to compile the source files
    echo "Compiling and building NAG17 files into libnag.a..."
    cd "$build_dir" || { echo "Failed to change directory to $build_dir"; exit 1; }
    ./compile
    ./buildlib
    cd ..
    mv libnag.a ..
    cd ..
    rm -r nag17
    cd ..
}

# Get the OS type
os_type=$(get_os_type)

# Compile the NAG17 library
compile_nag17 "$os_type"

# Compile the Slatec library
compile_slatec "$os_type"

# Install NetCDF based on the OS type
install_netcdf "$os_type"

# Install LAPACK based on the OS type
install_lapack_and_openblas "$os_type"

#compile the libbasis.a library from the makefile
make lib


#  Compilation flags for LAPACK on mac OS
#  LDFLAGS="-L/opt/homebrew/opt/lapack/lib"
#  CPPFLAGS="-I/opt/homebrew/opt/lapack/include"