#include "bus.h"

Bus::Bus()
{
    // Clearing content
    ram.fill(0x00);

    cpu.ConnectBus(this);
}

Bus::~Bus() {}

void Bus::write(uint16_t address, uint8_t data)
{
    if(address >= 0x0000 && address <= 0xFFFF)
    {
        ram[address] = data;
    }

}

uint8_t Bus::read(uint16_t address, bool bReadOnly)
{
    if(address >= 0x0000 && address <= 0xFFFF)
    {
        return ram[address];
    }

    return 0x00;
}
