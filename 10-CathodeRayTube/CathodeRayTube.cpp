// Cathode-Ray Tube
// https://adventofcode.com/2022/day/10

#include <array>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

class Instruction
{
    private:
        std::string command;
        int value;

    public:
        Instruction(std::string line)
        {
            size_t space = line.find(' ');
            command = line.substr(0, space);
            if (space != std::string::npos)
                value = stoi(line.substr(space+1));
        }

        std::string getCommand(void) { return command; }
        int getValue(void) { return value; }
};

class Crt {
    private:
        std::array<std::string, 6> display;
        int line;
        int cursor;

    public:
        Crt()
        {
            line = 0;
            cursor = 0;
        }

        void drawPixel(int x)
        {
            char pixel = ((cursor >= x-1) && (cursor <= x+1)) ? '#' : '.';
            display[line].append(1, pixel);
            cursor++;
            if (cursor == 40)
            {
                cursor = 0;
                line++;
            }
        }

        void show()
        {
            for(const std::string &line : display)
                std::cout << line << std::endl;
        }
};

class Cpu
{
    private:
        int x;
        int cycle;
        int next_signal;
        int sum_signal_strength;
        Crt *crt;

        void nextCycle(void)
        {
            cycle++;
            crt->drawPixel(x);
            if (cycle == next_signal)
            {
                int signal = cycle * x;
                sum_signal_strength += signal;
                next_signal += 40;
            }
        }

    public:
        Cpu(Crt *crt)
        {
            x = 1;
            cycle = 0;
            next_signal = 20;
            sum_signal_strength = 0;
            this->crt = crt;
        }

        int getX(void) { return x; }
        int getCycle(void) { return cycle; }
        int getSumOfSignalStrengths() { return sum_signal_strength; }

        void process(Instruction instruction)
        {
            nextCycle();
            if (instruction.getCommand() == "addx")
            {
                nextCycle();
                x += instruction.getValue();
            }
        }
};

void processInput(Cpu &cpu)
{
    std::ifstream input("input.txt");
    std::string line;
    while (std::getline(input, line))
        cpu.process(Instruction(line));
}

int main(void)
{
    Crt crt;
    Cpu cpu(&crt);

    processInput(cpu);

    std::cout << "Part 1: " << cpu.getSumOfSignalStrengths() << std::endl;
    std::cout << "Part 2:" << std::endl;
    crt.show();
}
