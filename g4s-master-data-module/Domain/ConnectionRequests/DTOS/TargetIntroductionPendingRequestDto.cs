using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class TargetIntroductionPendingRequestDto : TargetPendingRequestDto
    {
        public PlayerDto MiddleMan { get; set; }
        public string MiddleManToTargetMessage { get; set; }

        public TargetIntroductionPendingRequestDto(string id, PlayerDto player, PlayerDto target, string playerToTargetMessage, PlayerDto middleMan, string middleManToTargetMessage) 
            : base(id, player, target, playerToTargetMessage)
        {
            MiddleMan = middleMan;
            MiddleManToTargetMessage = middleManToTargetMessage;
        }
    }
}
