#include <fstream>
#include <iostream>
#include <NTL/vector.h>
#include <NTL/matrix.h>

void usage(char *prog_name) {
    std::cout << "USAGE: " << prog_name << " FILE" << std::endl
              << "Verify that NTL can load the contents of FILE as a Vec<Vec<Mat<long>>>." << std::endl
              ;
}

int main(int argc, char **argv) {
    if(argc != 2) {
        usage(argv[0]);
        return 1;
    }

    std::fstream fs;
    NTL::Vec< NTL::Vec< NTL::Mat<long> > > bp;

    fs.open(argv[1]);
    fs >> bp;
    fs.close();

    std::cout << bp << std::endl;
    return 0;
}
