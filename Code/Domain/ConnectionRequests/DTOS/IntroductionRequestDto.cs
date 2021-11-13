using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class IntroductionRequestDto : ConnectionRequestDto
    {
        public string MiddleMan { get; set; }
        public string PlayerToMiddleManMessage { get; set; }
        public string MiddleManToTargetMessage { get; set; }

        public IntroductionRequestDto(string id, string player, string middleMan, string target, string playerToTargetMessage,
            string playerToMiddleManMessage, string middleManToTargetMessage, string currentStatus, int strength, ICollection<string> tags)
            : base(id, player, target, playerToTargetMessage, currentStatus, strength, tags)
        {
            MiddleMan = middleMan;
            PlayerToMiddleManMessage = playerToMiddleManMessage;
            MiddleManToTargetMessage = middleManToTargetMessage;
        }
    }
}
