// readPDZ.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include <fstream>

void readRecord0(int);
void readRecord1(int);
void readRecord2(int);
void readRecord3(int);

std::ifstream file;

int main()
{
	bool eof = false;
	int offset = 0;
	short  record;
	int  recordSize;
	
	std::ifstream file(fileName, std::ios::in | std::ios::binary);

	while (!file.eof) {
		file.seekg(offset);			// to start of next record
		file.read((char *)record, sizeof(short));
		file.read((char *)recordSize, sizeof(int);
		switch (record) {
		case 0: readRecord0(recordSize);  break;
		case 1: readRecord1(recordSize);  break;
		case 2: readRecord2(recordSize);  break;
		case 3: readRecord3(recordSize);  break;
		// more if needed
		default: cout << "wrong record number";
		}
		offset += recordSize + 6;	// the record number and size are included separately
	}
}

void readRecord0(int size) {	// one for each record
	// pull from a what is needed. or read parts at a time
}

void readRecord1(int size) {	// one for each record
	// pull from a what is needed.or read parts at a time
}

void readRecord2(int size) {	// one for each record
	// pull from a what is needed.or read parts at a time
}

void readRecord3(int size) {	// one for each record
	SYSTENTIME pdztime;
	int		illumLength;
	char   a[10000];
	file.read(a, 88);	// skip a bunch, but these are all fixed size
	file.read(pdztime, 16);
	file.read(a, 10);
	file.read((char*)illumLength, 4);	// read how lo illumination is
	if (illumLength > 0)
		file.read(a, illumLength); // read the string

	file.read(a, 4 * 2048);	// read the spectrum
	for (int i = 0; i < 2048; i++) {
		res[i] = (a[4 * i]);
	}
}





