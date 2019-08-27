/*
    License (OLC-3)
    ~~~~~~~~~~~~~~~
    Copyright 2018-2019 OneLoneCoder.com
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions or derivations of source code must retain the above
    copyright notice, this list of conditions and the following disclaimer.
    2. Redistributions or derivative works in binary form must reproduce
    the above copyright notice. This list of conditions and the following
    disclaimer must be reproduced in the documentation and/or other
    materials provided with the distribution.
    3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived
    from this software without specific prior written permission.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    This file is based on David Barr's work

    Relevant Video: https://youtu.be/8XmxKPJDGU0
    Links

    Datasheet: http://archive.6502.org/datasheets/rockwell_r650x_r651x.pdf

    ~~~~~
    YouTube:	https://www.youtube.com/javidx9
                https://www.youtube.com/javidx9extra
    Discord:	https://discord.gg/WhwHUMV
    Twitter:	https://www.twitter.com/javidx9
    Twitch:	https://www.twitch.tv/javidx9
    GitHub:	https://www.github.com/onelonecoder
    Patreon:	https://www.patreon.com/javidx9
    Homepage:	https://www.onelonecoder.com
    ~~~~~~
*/

#ifndef CPU_H
#define CPU_H

#include <cstdint>
#include <string>
#include <vector>

class Bus;

class CPU
{
public:
    CPU();
    ~CPU();

public:
    enum CPU_FLAGS
    {
        C = 1 << 0, //Carry Bit
        Z = 1 << 1, //Zero
        I = 1 << 2, //Disable Interrupts
        D = 1 << 3, //Decimal Mode
        B = 1 << 4, //Break
        U = 1 << 5, //Unused
        V = 1 << 6, //Overflow
        N = 1 << 7, //Negative
    };

    //CPU Core registers
    uint8_t a       = 0x00;     //Acumulator
    uint8_t x       = 0x00;     //X Register
    uint8_t y       = 0x00;     //Y Register
    uint8_t stkp    = 0x00;     //Stack Pointer
    uint16_t pc     = 0x0000;   //Progam Counter
    uint8_t status  = 0x00;     //Status Register

public:
    void ConnectBus(Bus *bus) { this->bus = bus; }

    void clock(); //Perform one clock cycle
    void reset(); //Reset Interrupt - Forces CPU into known state
    void irq();	  //Interrupt Request - Executes an instruction at a specific location
    void nmi();	  //Non-Maskable Interrupt Request - As above, but cannot be disabled

private:
    Bus*     bus        = nullptr;
    uint8_t  fetched    = 0x00;   // Represents the working input value to the ALU
    uint16_t temp       = 0x0000; // A convenience variable used everywhere
    uint16_t address_abs= 0x0000; // All used memory addresses end up in here
    uint16_t address_rel= 0x00;   // Represents absolute address following a branch
    uint8_t  opcode     = 0x00;   // Is the instruction byte
    uint8_t  cycles     = 0;	  // Counts how many cycles the instruction has remaining
    uint32_t clock_count= 0;	  // A global accumulation of the number of clocks

    struct INSTRUCTION
    {
        std::string name; //A textual representation of the instruction
        uint8_t     (CPU::*operate )(void) = nullptr; //A function pointer to the implementation of the opcode
        uint8_t     (CPU::*addrmode)(void) = nullptr; //A function pointer to the implementation of the addressing mechanism used by the instruction
        uint8_t     cycles = 0; //number of clock cycles the CPU requires to perform the instruction
    };

    std::vector<INSTRUCTION> lookup;

    uint8_t read(uint16_t address);
    void    write(uint16_t address, uint8_t data);

    // The read location of data can come from two sources, a memory address, or
    // its immediately available as part of the instruction. This function decides
    // depending on address mode of instruction byte
    uint8_t fetch();

    // Functions to access status registers
    uint8_t GetFlag(CPU_FLAGS flag);
    void    SetFlag(CPU_FLAGS, bool value);

    // Addressing Modes =============================================
    // The 6502 has a variety of addressing modes to access data in
    // memory, some of which are direct and some are indirect (like
    // pointers in C++). Each opcode contains information about which
    // addressing mode should be employed to facilitate the
    // instruction, in regards to where it reads/writes the data it
    // uses. The address mode changes the number of bytes that
    // makes up the full instruction, so we implement addressing
    // before executing the instruction, to make sure the program
    // counter is at the correct location, the instruction is
    // primed with the addresses it needs, and the number of clock
    // cycles the instruction requires is calculated. These functions
    // may adjust the number of cycles required depending upon where
    // and how the memory is accessed, so they return the required
    // adjustment.
    uint8_t IMP();  uint8_t IMM();
    uint8_t ZP0();  uint8_t ZPX();
    uint8_t ZPY();  uint8_t REL();
    uint8_t ABS();  uint8_t ABX();
    uint8_t ABY();  uint8_t IND();
    uint8_t IZX();  uint8_t IZY();

    // Opcodes ======================================================
    // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
    // have not modelled "unofficial" opcodes. As each opcode is
    // defined by 1 byte, there are potentially 256 possible codes.
    // The opcodes listed here are the official ones, meaning that
    // the functionality of the chip when provided with these codes
    // is as the developers intended it to be.
    // Unofficial codes will of course also influence the CPU
    // circuitry in interesting ways, and can be exploited to gain
    // additional functionality!
    //
    // These functions return 0 normally, but some are capable of
    // requiring more clock cycles when executed under certain
    // conditions combined with certain addressing modes. If that is
    // the case, they return 1.
    uint8_t ADC();  uint8_t AND();  uint8_t ASL();  uint8_t BCC();
    uint8_t BCS();  uint8_t BEQ();  uint8_t BIT();  uint8_t BMI();
    uint8_t BNE();  uint8_t BPL();  uint8_t BRK();  uint8_t BVC();
    uint8_t BVS();  uint8_t CLC();  uint8_t CLD();  uint8_t CLI();
    uint8_t CLV();  uint8_t CMP();  uint8_t CPX();  uint8_t CPY();
    uint8_t DEC();  uint8_t DEX();  uint8_t DEY();  uint8_t EOR();
    uint8_t INC();  uint8_t INX();  uint8_t INY();  uint8_t JMP();
    uint8_t JSR();  uint8_t LDA();  uint8_t LDX();  uint8_t LDY();
    uint8_t LSR();  uint8_t NOP();  uint8_t ORA();  uint8_t PHA();
    uint8_t PHP();  uint8_t PLA();  uint8_t PLP();  uint8_t ROL();
    uint8_t ROR();  uint8_t RTI();  uint8_t RTS();  uint8_t SBC();
    uint8_t SEC();  uint8_t SED();  uint8_t SEI();  uint8_t STA();
    uint8_t STX();  uint8_t STY();  uint8_t TAX();  uint8_t TAY();
    uint8_t TSX();  uint8_t TXA();  uint8_t TXS();  uint8_t TYA();

    // Redirect all invalid instructions to this
    uint8_t XXX();

};

#endif // CPU_H
