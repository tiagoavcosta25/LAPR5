using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class AcceptRequestDto
    {
        public string Player { get; set; }
        public string Target { get; set; }
        public int Strength { get; set; }
        public ICollection<string> Tags { get; set; }

        public AcceptRequestDto(string player, string target, int strength, ICollection<string> tags)
        {
            Player = player;
            Target = target;
            Strength = strength;
            Tags = tags;
        }
    }
}
