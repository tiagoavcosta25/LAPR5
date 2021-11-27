using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class AcceptRequestDto
    {
        public string Id { get; set; }
        public int Strength { get; set; }
        public ICollection<string> Tags { get; set; }

        public AcceptRequestDto(string id, int strength, ICollection<string> tags)
        {
            Id = id;
            Strength = strength;
            Tags = tags;
        }
    }
}
