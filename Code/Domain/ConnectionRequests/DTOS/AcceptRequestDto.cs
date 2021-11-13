using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class AcceptRequestDto
    {
        public int Strength { get; set; }
        public ICollection<string> Tags { get; set; }

        public AcceptRequestDto(int strength, ICollection<string> tags)
        {
            Strength = strength;
            Tags = tags;
        }
    }
}
