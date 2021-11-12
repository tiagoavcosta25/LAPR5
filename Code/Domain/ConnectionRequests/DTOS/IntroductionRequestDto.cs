using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class IntroductionRequestDto
    {
        public string Id { get; set; }
        public string Player { get; set; }
        public string MiddleMan { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public string PlayerToMiddleManMessage { get; set; }
        public string MiddleManToTargetMessage { get; set; }
        public string CurrentStatus { get; set; }

        public IntroductionRequestDto(string id, string player, string middleMan, string target, string playerToTargetMessage, 
            string playerToMiddleManMessage, string middleManToTargetMessage, string currentStatus)
        {
            Id = id;
            Player = player;
            MiddleMan = middleMan;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            PlayerToMiddleManMessage = playerToMiddleManMessage;
            MiddleManToTargetMessage = middleManToTargetMessage;
            CurrentStatus = currentStatus;
        }
    }
}
