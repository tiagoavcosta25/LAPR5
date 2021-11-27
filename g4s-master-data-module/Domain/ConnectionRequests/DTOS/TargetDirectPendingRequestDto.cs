using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class TargetDirectPendingRequestDto : TargetPendingRequestDto
    {
        public TargetDirectPendingRequestDto(string id, PlayerDto player, PlayerDto target, string playerToTargetMessage) : base(id, player, target, playerToTargetMessage)
        {
        }
    }
}
