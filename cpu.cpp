#include "cpu.h"
#include "bus.h"

CPU::CPU()
{
    using a = CPU;
    lookup =
    {
        { "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
        { "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
        { "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
        { "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
        { "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
    };
}

CPU::~CPU()
{

}

uint8_t CPU::read(uint16_t address)
{
    return bus->read(address);
}

void CPU::write(uint16_t address, uint8_t data)
{
    bus->write(address, data);
}

void CPU::clock()
{
    //When cycle is in 0, it means that there are no current instructions
    //to be executed, so it can read a new one
    if(cycles == 0)
    {
        //get the opcode current memory location
        opcode = read(pc++);

        //Getting the required cycles for the instruction
        cycles = lookup[opcode].cycles;
        uint8_t extra_cycle1 = (this->*lookup[opcode].addrmode)();
        uint8_t extra_cycle2 = (this->*lookup[opcode].operate)();

        //If both instruction and address function return 1, then an additional
        //clock cycle is required.
        cycles += (extra_cycle1 & extra_cycle2);
    }

    cycles--;
}

// Forces CPU into a known state,
void CPU::reset()
{
    // Reset internal registers
    a = 0;
    x = 0;
    y = 0;
    stkp = 0xFD;
    status = 0x00 | U;

    // Get address to set program counter to
    address_abs = 0xFFFC;
    uint16_t lo = read(address_abs + 0);
    uint16_t hi = read(address_abs + 1);

    // Set it
    pc = (hi << 8) | lo;

    // Clear internal helper variables
    address_rel = 0x0000;
    address_abs = 0x0000;
    fetched = 0x00;

    // Reset takes time
    cycles = 8;
}

// IRQs can happen at any time, but
// you dont want them to be destructive to the operation of the running
// program. Therefore the current instruction is allowed to finish and
// then the current program counter is stored on the stack. Then the
// current status register is stored on the stack. When the routine
// that services the interrupt has finished, the status register
// and program counter can be restored to how they where before it
// occurred. This is impemented by the "RTI" instruction. Once the IRQ
// has happened, in a similar way to a reset, a programmable address
// is read form hard coded location 0xFFFE, which is subsequently
// set to the program counter.
void CPU::irq()
{
    //Interrupt requests only happen if the "disable interrupt" flag is 0.
    if (GetFlag(I) == 0)
    {
        //Push the program counter to the stack.
        //It's 16-bits and that takes two pushes
        write(STACK_OFFSET + stkp, (pc >> 8) & 0x00FF);
        stkp--;
        write(STACK_OFFSET + stkp, pc & 0x00FF);
        stkp--;

        //Then Push the status register to the stack
        SetFlag(B, 0);
        SetFlag(U, 1);
        SetFlag(I, 1);
        write(STACK_OFFSET + stkp, status);
        stkp--;

        //Read new program counter location from fixed address
        address_abs = 0xFFFE;
        uint16_t lo = read(address_abs + 0);
        uint16_t hi = read(address_abs + 1);
        pc = (hi << 8) | lo;

        // IRQs take time
        cycles = 7;
    }
}

// A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
// same way as a regular IRQ, but reads the new program counter address
// form location 0xFFFA.
void CPU::nmi()
{
    write(STACK_OFFSET + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(STACK_OFFSET + stkp, pc & 0x00FF);
    stkp--;

    SetFlag(B, 0);
    SetFlag(U, 1);
    SetFlag(I, 1);
    write(STACK_OFFSET + stkp, status);
    stkp--;

    address_abs = 0xFFFA;
    uint16_t lo = read(address_abs + 0);
    uint16_t hi = read(address_abs + 1);
    pc = (hi << 8) | lo;

    cycles = 8;
}

void CPU::SetFlag(CPU_FLAGS flag, bool value)
{
    if(value)
    {
        status |= flag;
    }
    else
    {
        status &= flag;
    }
}

uint8_t CPU::GetFlag(CPU_FLAGS flag)
{
    return ((status & flag) > 0) ? 1 : 0;
}

// This function sources the data used by the instruction.
// Some instructions dont have to fetch data as the source is implied
// by the instruction.
// "fetched" is a variable global to the CPU, and is set by calling this
// function. It also returns it for convenience.
uint8_t CPU::fetch()
{
    if(!(lookup[opcode].addrmode == &CPU::IMP))
    {
        fetched = read(address_abs);
    }

    return fetched;
}


///////////////////////////////////////////////////////////////////////////////
// ADDRESSING MODES

// The 6502 can address between 0x0000 - 0xFFFF. The high byte is often referred
// to as the "page", and the low byte is the offset into that page.
//
// Several addressing modes have the potential to require an additional clock
// cycle if they cross a page boundary. This is combined with several instructions
// that enable this additional clock cycle. So each addressing function returns
// a flag saying it has potential, as does each instruction.


// Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
uint8_t CPU::IMP()
{
    fetched = a;
    return 0;
}

// Address Mode: Immediate
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
uint8_t CPU::IMM()
{
    address_abs = pc++;
    return 0;
}

// Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
uint8_t CPU::ZP0()
{
    address_abs = read(pc++);
    address_abs &= 0x00FF;
    return 0;
}

// Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
uint8_t CPU::ZPX()
{
    address_abs = (read(pc++) + x);
    address_abs &= 0x00FF;
    return 0;
}

// Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
uint8_t CPU::ZPY()
{
    address_abs = (read(pc++) + y);
    address_abs &= 0x00FF;
    return 0;
}


// Address Mode: Relative
// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
uint8_t CPU::REL()
{
    address_rel = read(pc++);
    if (address_rel & 0x80)
    {
        address_rel |= 0xFF00;
    }
    return 0;
}


// Address Mode: Absolute
// A full 16-bit address is loaded and used
uint8_t CPU::ABS()
{
    uint16_t lo = read(pc++);
    uint16_t hi = read(pc++);

    address_abs = (hi << 8) | lo;

    return 0;
}


// Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
uint8_t CPU::ABX()
{
    uint16_t lo = read(pc++);
    uint16_t hi = read(pc++);

    address_abs = (hi << 8) | lo;
    address_abs += x;

    return (((address_abs & 0xFF00) != (hi << 8)) > 0) ? 1 : 0;
}


// Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
uint8_t CPU::ABY()
{
    uint16_t lo = read(pc++);
    uint16_t hi = read(pc++);

    address_abs = (hi << 8) | lo;
    address_abs += y;

    return ((address_abs & 0xFF00) != (hi << 8)) ? 1 : 0;
}

// Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is 0xFF, then to read the high byte of the actual address
// we need to cross a page boundary. This doesnt actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address
uint8_t CPU::IND()
{
    uint16_t ptr_lo = read(pc++);
    uint16_t ptr_hi = read(pc++);

    uint16_t ptr = (ptr_hi << 8) | ptr_lo;

    if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
    {
        address_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
    }
    else // Behave normally
    {
        address_abs = (read(ptr + 1) << 8) | read(ptr + 0);
    }

    return 0;
}


// Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
uint8_t CPU::IZX()
{
    uint16_t t = read(pc++);

    uint16_t lo = read((uint16_t)(t + (uint16_t)x) & 0x00FF);
    uint16_t hi = read((uint16_t)(t + (uint16_t)x + 1) & 0x00FF);

    address_abs = (hi << 8) | lo;

    return 0;
}


// Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
uint8_t CPU::IZY()
{
    uint16_t t = read(pc++);

    uint16_t lo = read(t & 0x00FF);
    uint16_t hi = read((t + 1) & 0x00FF);

    address_abs = (hi << 8) | lo;
    address_abs += y;

    return ((address_abs & 0xFF00) != (hi << 8)) ? 1 : 0;
}




///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// The typical order of events is:
// 1) Fetch the data you are working with
// 2) Perform calculation
// 3) Store the result in desired place
// 4) Set Flags of the status register
// 5) Return if instruction has potential to require additional clock cycle

// Add with Carry In
// Function:    A = A + M + C
// Flags Out:   C, V, N, Z
uint8_t CPU::ADC()
{
    // Grab the data that we are adding to the accumulator
    fetch();

    // Add is performed in 16-bit domain for emulation to capture any
    // carry bit, which will exist in bit 8 of the 16-bit word
    uint16_t temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)GetFlag(C);

    // If the result is > 255 there is an overflow setting the carry bit. This allows
    // us to chain together ADC instructions to add numbers larger than 8-bits.
    SetFlag(C, temp > 255);

    // The Zero flag is set if the result is 0
    SetFlag(Z, (temp & 0x00FF) == 0);

    // The negative flag is set to the most significant bit of the result
    SetFlag(N, temp & 0x80);

    // A  M  R | V | A^R | A^M |~(A^M) |
    // 0  0  0 | 0 |  0  |  0  |   1   |
    // 0  0  1 | 1 |  1  |  0  |   1   |
    // 0  1  0 | 0 |  0  |  1  |   0   |
    // 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
    // 1  0  0 | 0 |  1  |  1  |   0   |
    // 1  0  1 | 0 |  0  |  1  |   0   |
    // 1  1  0 | 1 |  1  |  0  |   1   |
    // 1  1  1 | 0 |  0  |  0  |   1   |
    //
    // We can see how the above equation calculates V, based on A, M and R. V was chosen
    // based on the following hypothesis:
    //       Positive Number + Positive Number = Negative Result -> Overflow
    //       Negative Number + Negative Number = Positive Result -> Overflow
    //       Positive Number + Negative Number = Either Result -> Cannot Overflow
    //       Positive Number + Positive Number = Positive Result -> OK! No Overflow
    //       Negative Number + Negative Number = Negative Result -> OK! NO Overflow
    SetFlag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);

    // Load the result into the accumulator (it's 8-bit dont forget!)
    a = temp & 0x00FF;

    // This instruction has the potential to require an additional clock cycle
    return 1;
}

// Subtraction with Borrow In
// Flags Out:   C, V, N, Z
uint8_t CPU::SBC()
{
    // We can reorganise our data to use the same computation for addition
    // but now for subtraction by just multiplying the data by -1, i.e. make it negative
    //
    // A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
    //
    // The range is actually unimportant, because if I take the value 15, and add 251
    // to it, given we wrap around at 256, the result is 10, so it has effectively
    // subtracted 5, which was the original intention. (15 + 251) % 256 = 10
    //
    // Note that the equation above used (1-C), but this got converted to + 1 + C.
    // This means we already have the +1, so all we need to do is invert the bits
    // of M, the data(!) therefore we can simply add, exactly the same way we did
    // before.


    fetch();

    // Operating in 16-bit domain to capture carry out

    // Invert the bottom 8 bits with bitwise xor
    uint16_t value = ((uint16_t)fetched) ^ 0x00FF;

    uint16_t temp = (uint16_t)a + value + (uint16_t)GetFlag(C);
    SetFlag(C, temp & 0xFF00);
    SetFlag(Z, ((temp & 0x00FF) == 0));
    SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
    SetFlag(N, temp & 0x0080);
    a = temp & 0x00FF;
    return 1;
}

//Push Accumulator to Stack
uint8_t CPU::PHA()
{
    //the CPU has hardcoded a base location to the stack
    write(STACK_OFFSET + stkp, a);
    stkp--;
    return 0;
}

//Pop Accumulator from Stack
uint8_t CPU::PLA()
{
    stkp++;
    a = read(STACK_OFFSET + stkp);
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return  0;
}

//Push Status register to stack
uint8_t CPU::PHP()
{
    write(STACK_OFFSET + stkp, status | B | U);
    SetFlag(B, false);
    SetFlag(U, false);
    stkp--;
    return 0;
}

//Pop status register from stack
uint8_t CPU::PLP()
{
    stkp++;
    status = read(STACK_OFFSET + stkp);
    SetFlag(U, true);
    return 0;
}


//Restore status after an interruption
uint8_t CPU::RTI()
{
    stkp++;
    status = read(STACK_OFFSET + stkp);
    status &= ~B;
    status &= ~U;

    stkp++;
    pc = (uint16_t)read(STACK_OFFSET + stkp);
    stkp++;
    pc |= (uint16_t)read(STACK_OFFSET + stkp) << 8;

    return 0;
}

uint8_t CPU::RTS()
{
    stkp++;
    pc = (uint16_t)read(STACK_OFFSET + stkp);
    stkp++;
    pc |= (uint16_t)read(STACK_OFFSET + stkp) << 8;

    pc++;
    return 0;
}

uint8_t CPU::ROL()
{
    fetch();
    uint16_t temp = (uint16_t)(fetched << 1) | GetFlag(C);
    SetFlag(C, temp & 0xFF00);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &CPU::IMP)
    {
        a = temp & 0x00FF;
    }
    else
    {
        write(address_abs, temp & 0x00FF);
    }
    return 0;
}

uint8_t CPU::ROR()
{
    fetch();
    uint16_t temp = (uint16_t)(GetFlag(C) << 7) | (fetched >> 1);
    SetFlag(C, fetched & 0x01);
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &CPU::IMP)
    {
        a = temp & 0x00FF;
    }
    else
    {
        write(address_abs, temp & 0x00FF);
    }
    return 0;
}

//Bitwise Logic AND
uint8_t CPU::AND()
{
    fetch();
    a &= fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}

//Bitwise Logic OR
uint8_t CPU::ORA()
{
    fetch();
    a |= fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x00);
    return 1;
}

//Branch if Carry is set
uint8_t CPU::BCS()
{
    if (GetFlag(C) == 1)
    {
        cycles++;
        address_abs = pc + address_rel;

        //if needs to cross a page boundary, it needs another cycle
        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

//Branch if Carry is clear
uint8_t CPU::BCC()
{
    if (GetFlag(C) == 0)
    {
        cycles++;
        address_abs = pc + address_rel;

        if((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

//Branch if Negative
uint8_t CPU::BMI()
{
    if (GetFlag(N) == 1)
    {
        cycles++;
        address_abs = pc + address_rel;

        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

// Branch if Positive
uint8_t CPU::BPL()
{
    if (GetFlag(N) == 0)
    {
        cycles++;
        address_abs = pc + address_rel;

        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

// Branch if Equal
uint8_t CPU::BEQ()
{
    if (GetFlag(Z) == 1)
    {
        cycles++;
        address_abs = pc + address_rel;

        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

// Branch if not Equal
uint8_t CPU::BNE()
{
    if (GetFlag(Z) == 0)
    {
        cycles++;
        address_abs = pc + address_rel;

        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

// Branch if Overflow Set
uint8_t CPU::BVS()
{
    if (GetFlag(V) == 1)
    {
        cycles++;
        address_abs = pc + address_rel;

        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

// Branch if Overflow clear
uint8_t CPU::BVC()
{
    if (GetFlag(V) == 0)
    {
        cycles++;
        address_abs = pc + address_rel;

        if ((address_abs & 0xFF00) != (pc & 0xFF00))
        {
            cycles++;
        }
        pc = address_abs;
    }
    return 0;
}

// Clear Carry flag
uint8_t CPU::CLC()
{
    SetFlag(C, false);
    return 0;
}


// Clear Decimal flag
uint8_t CPU::CLD()
{
    SetFlag(D, false);
    return 0;
}

// Disable Interrupts / Clear Interrupt flag
uint8_t CPU::CLI()
{
    SetFlag(I, false);
    return 0;
}


// Clear Overflow Flag
uint8_t CPU::CLV()
{
    SetFlag(V, false);
    return 0;
}

//Set Carry Flag
uint8_t CPU::SEC()
{
    SetFlag(C, true);
    return 0;
}


//Set Decimal Flag
uint8_t CPU::SED()
{
    SetFlag(D, true);
    return 0;
}

//Set Interrupt Flag / Enable Interrupts
uint8_t CPU::SEI()
{
    SetFlag(I, true);
    return 0;
}

//Store Accumulator in memory
uint8_t CPU::STA()
{
    write(address_abs, a);
    return 0;
}

//Store X register in memory
uint8_t CPU::STX()
{
    write(address_abs, x);
    return 0;
}

//Store Y register in memory
uint8_t CPU::STY()
{
    write(address_abs, y);
    return 0;
}

//Transfer ACC to X register
uint8_t CPU::TAX()
{
    x = a;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}

//Transfer ACC to Y register
uint8_t CPU::TAY()
{
    y = a;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
    return 0;
}

//Transfer Stack pointer to X register
uint8_t CPU::TSX()
{
    x = stkp;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}

//Transfers X register to ACC
uint8_t CPU::TXA()
{
    a = x;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}

//Transfers X register to stack pointer
uint8_t CPU::TXS()
{
    stkp = x;
    return 0;
}

//Transfers Y register to ACC
u_int8_t CPU::TYA()
{
    a = y;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}

//Arithmetic shift left
uint8_t CPU::ASL()
{
    fetch();
    uint16_t temp = (uint16_t)fetched << 1;
    SetFlag(C, (temp & 0xFF00) > 0);
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, temp & 0x80);

    if(lookup[opcode].addrmode == &CPU::IMP)
    {
        a = temp & 0x00FF;
    }
    else
    {
        write(address_abs, temp & 0x00FF);
    }

    return 0;
}

//Skip
uint8_t CPU::NOP()
{
    //Oddly, not all NOP are equal
    //https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
    //need to add depending on game compatibility
    switch (opcode)
    {
        case 0x1C: case 0x3C: case 0x5C:
        case 0x7C: case 0xDC: case 0xFC:
            return 1;
    }

    return 0;
}

//Defaul case for illegal instructions
uint8_t CPU::XXX()
{
    return 0;
}
