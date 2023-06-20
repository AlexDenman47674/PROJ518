using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandFunctionImplementation
{
    class Program
    {
        static void Main(string[] args)
        {
            //The Rand function provided within the C# IDE generates a seed based around the system clock of the PC being used 
            //This can change if the application is a .NET core file, which generates a seed based on a thread specific pseudorandom generator within the system
            //For a closer match to true random, the file format used for this algorithm is a .NET framework
            //In addition, the Random function provided by C# also allows for an inputted seed that will be used in place of the system clock

            //Declares the list to store generated values
            List<double> ReturnValues = new List<double>();
            
            //Implementation 1 of the Random function
            RandImplementation1(ref ReturnValues);
        }

        static void RandImplementation1(ref List<double>Values)
        {
            //The random class can generate a variety of pseudorandom variables
            //The .next(x) function will generate positive integers between 0 and x

            Values.Clear();

            //By providing no seed value, the rand variable will generate a seed based on the current time
            Random rand = new Random();

            for (int i = 1; i <= 100; i++)
            {
                //Values between 0 and 100 are generated
                Values.Add(rand.Next(100));
            }
        }
    }
}
