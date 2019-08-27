#ifndef BUS_H
#define BUS_H

#include <cstdint>
#include <array>

#include "cpu.h"

class Bus
{
public:
    Bus();
    ~Bus();

public: //Devices attached to bus
    CPU cpu;

    //Fake RAM
    std::array<uint8_t, 64 * 1024> ram;

public: //Bus operations
    void write(uint16_t address, uint8_t data);
    uint8_t read(uint16_t address, bool bReadOnly = false);
};

#endif // BUS_H
