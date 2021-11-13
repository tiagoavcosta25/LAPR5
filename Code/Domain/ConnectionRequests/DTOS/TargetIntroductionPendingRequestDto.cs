using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class TargetIntroductionPendingRequestDto : TargetPendingRequestDto
    {
        public string MiddleMan { get; set; }
        public string MiddleManToTargetMessage { get; set; }

        public TargetIntroductionPendingRequestDto(string player, string target, string playerToTargetMessage, string middleMan, string middleManToTargetMessage)
            : base(player, target, playerToTargetMessage)
        {
            MiddleMan = middleMan;
            MiddleManToTargetMessage = middleManToTargetMessage;
        }
    }
}
