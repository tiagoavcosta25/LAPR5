using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class CreatingDirectRequestAutoDto
    {
        public string Player { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public int Strength { get; set; }
        public ICollection<string> Tags { get; set; }

        public CreatingDirectRequestAutoDto(string player, string target, string playerToTargetMessage,int strength, ICollection<string> tags)
        {
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            Strength = strength;
            Tags = tags;
        }
    }
}
