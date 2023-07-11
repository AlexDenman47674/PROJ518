using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Security.Cryptography;

using Newtonsoft.Json;
using System.IO;

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

            //Declares the lists to store generated values
            List<double> ReturnValues = new List<double>();
            List<string> ReturnCardValues = new List<string>();
            
            //Implementation 1 of the Random function
            RandImplementation1(ref ReturnValues);

            Console.WriteLine("Implementation 1");

            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            //Writes data to JSON
            string Randjson1 = JsonConvert.SerializeObject(ReturnValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/RandVer1.json", Randjson1);

            //Implementation 2 of the Random function
            RandImplementation2(ref ReturnValues);

            Console.WriteLine("Implementation 2");

            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            //Writes data to JSON
            string Randjson2 = JsonConvert.SerializeObject(ReturnValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/RandVer2.json", Randjson2);

            //Implementation 3 of the Random function

            Console.WriteLine("Implementation 3");

            byte[] ByteValues = RandImplementation3(500);
            ReturnValues.Clear();

            for (int i = 0; i <= ByteValues.Count() - 1; i++)
            {
                Console.WriteLine(ByteValues[i]);
                ReturnValues.Add(ByteValues[i]);
            }

            //Writes data to JSON
            string Randjson3 = JsonConvert.SerializeObject(ReturnValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/RandVer3.json", Randjson3);


            //Simulation 1 of a coin flip
            RandCoinSimulation1(ref ReturnValues);

            Console.WriteLine("Coin Simulation 1");

            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            //Writes data to JSON
            string Coinjson1 = JsonConvert.SerializeObject(ReturnValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/C#CoinSim1.json", Coinjson1);

            //Simulation 2 of a coin flip
            RandCoinSimulation2(ref ReturnValues);

            Console.WriteLine("Coin Simulation 2");

            for (int i = 0; i <= ReturnValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnValues[i]);
            }

            //Writes data to JSON
            string Coinjson2 = JsonConvert.SerializeObject(ReturnValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/C#CoinSim2.json", Coinjson2);

            //Simulation 1 of a card draw
            RandCardSim1(ref ReturnCardValues);

            Console.WriteLine("Cards Simulation 1");

            for (int i = 0; i <= ReturnCardValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnCardValues[i]);
            }

            //Writes data to JSON
            string Cardjson1 = JsonConvert.SerializeObject(ReturnCardValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/C#CardSim1.json", Cardjson1);

            //Simulation 2 of a card draw
            RandCardSim1(ref ReturnCardValues);

            Console.WriteLine("Cards Simulation 2");

            for (int i = 0; i <= ReturnCardValues.Count() - 1; i++)
            {
                Console.WriteLine(ReturnCardValues[i]);
            }

            //Writes data to JSON
            string Cardjson2 = JsonConvert.SerializeObject(ReturnCardValues.ToArray());

            System.IO.File.WriteAllText(@"D:/Github/PROJ518/C#_Rand_Function/RandFunctionOutput/C#CardSim2.json", Cardjson2);

            Console.ReadLine();
        }

        static void RandImplementation1(ref List<double>Values)
        {
            //The random class can generate a variety of pseudorandom variables
            //The .next(x) function will generate positive integers between 0 and x

            Values.Clear();

            //By providing no seed value, the rand variable will generate a seed based on the current time
            Random rand = new Random();

            for (int i = 1; i <= 500; i++)
            {
                //Values between 0 and 100 are generated
                Values.Add(rand.Next(100));
            }
        }

        static void RandImplementation2(ref List<double>Values)
        {
            //As with Implementation 1, the .next() function will be used to generate random values
            //However when creating the Random class the control seed 30/10/2000 will be used

            Values.Clear();

            Random rand = new Random(30102000);

            for (int i = 1; i <= 500; i++)
            {
                //Values between 0 and 100 are generated
                Values.Add(rand.Next(100));
            }
        }

        static byte[] RandImplementation3(int size)
        {
            //In additon to Random, C# also provides the RandomNumberGenerator function
            //Unlike the default Random, RandomNumberGenerator is a cryptographic generator
            //To access this generator the System.Security.Crytography library is required

            using (var generator = RandomNumberGenerator.Create())
            {
                //The array values stores the generated bytes created by RandomNumberGenerator
                var values = new byte[size];

                generator.GetBytes(values);

                return values;
            }
        }

        static void RandCoinSimulation1(ref List<double> Values)
        {
            //This function operates the same as the implementation 1 function
            //However the .next() values are adjusted to 0 (heads) and 1 (tails)

            Values.Clear();

            Random rand = new Random();

            for (int i = 1; i <= 500; i++)
            {
                //Values between 0 and 1 are generated
                Values.Add(rand.Next(2));
            }
        }

        static void RandCoinSimulation2(ref List<double> Values)
        {
            //This function operates the same as the simulation 1 function
            //However the standard seed 30/10/2000 is used in place of the system generated seed

            Values.Clear();

            Random rand = new Random(30102000);

            for (int i = 1; i <= 500; i++)
            {
                //Values between 0 and 1 are generated
                Values.Add(rand.Next(2));
            }
        }

        static void RandCardSim1(ref List<string> Values)
        {
            //This function operates the same as the implementation 1 function
            //however the .next() values are adjusted after each generation as if picking from a deck of cards
            //Originally the values are generated between 0 and 51 simulating a standard deck of 52 cards
            //Each card is bought into a 'Deck' list by reading from a deck JSON file
            //As cards are being removed from the 'Deck' the .next() max value decreases

            Values.Clear();

            //Creates the Deck list
            List<string> Deck = new List<string>();

            Random rand = new Random();

            int ChosenCard;

            //Reads the Deck JSON into the 'Deck' list
            using (StreamReader r = new StreamReader("D:/Github/PROJ518/C#_Rand_Function/RandFunctionInput/Deck.json"))
            {
                string json = r.ReadToEnd();
                Deck = JsonConvert.DeserializeObject<List<string>>(json);
            }

            for (int i = 0; i <= 51; i++)
            {
                ChosenCard = rand.Next(Deck.Count()-1);
                Values.Add(Deck[ChosenCard]);

                //Once a card is added, it must be removed from the deck
                Deck.RemoveAt(ChosenCard);
            }
        }

        static void RandCardSim2(ref List<string> Values)
        {
            //This function operates the same as simulation 1
            //however the control seed is used

            Values.Clear();

            //Creates the Deck list
            List<string> Deck = new List<string>();

            Random rand = new Random(30102000);

            int ChosenCard;

            //Reads the Deck JSON into the 'Deck' list
            using (StreamReader r = new StreamReader("D:/Github/PROJ518/C#_Rand_Function/RandFunctionInput/Deck.json"))
            {
                string json = r.ReadToEnd();
                Deck = JsonConvert.DeserializeObject<List<string>>(json);
            }

            for (int i = 0; i <= 51; i++)
            {
                ChosenCard = rand.Next(Deck.Count() - 1);
                Values.Add(Deck[ChosenCard]);

                //Once a card is added, it must be removed from the deck
                Deck.RemoveAt(ChosenCard);
            }
        }
    }
}
