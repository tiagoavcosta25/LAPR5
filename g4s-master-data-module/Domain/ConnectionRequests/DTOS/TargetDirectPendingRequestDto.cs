using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class TargetDirectPendingRequestDto : TargetPendingRequestDto
    {
        public TargetDirectPendingRequestDto(string player, string target, string playerToTargetMessage) : base(player, target, playerToTargetMessage)
        {
        }
    }
}
