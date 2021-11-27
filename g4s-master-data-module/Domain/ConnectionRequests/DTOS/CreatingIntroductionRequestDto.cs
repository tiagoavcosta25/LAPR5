using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class CreatingIntroductionRequestDto
    {
        public string Player { get; set; }
        public string MiddleMan { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public string PlayerToMiddleManMessage { get; set; }
        public string MiddleManToTargetMessage { get; set; }
        public int Strength { get; set;  }
        public ICollection<string> Tags { get; set; }

        public CreatingIntroductionRequestDto(string player, string middleMan, string target, string playerToTargetMessage,
            string playerToMiddleManMessage, string middleManToTargetMessage, int strength, ICollection<string> tags)
        {
            Player = player;
            MiddleMan = middleMan;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            PlayerToMiddleManMessage = playerToMiddleManMessage;
            MiddleManToTargetMessage = middleManToTargetMessage;
            Strength = strength;
            Tags = tags;
        }
    }
}
