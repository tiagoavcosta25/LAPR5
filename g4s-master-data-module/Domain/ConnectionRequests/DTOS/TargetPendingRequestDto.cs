using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public abstract class TargetPendingRequestDto
    {
        public string Player { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }

        protected TargetPendingRequestDto(string player, string target, string playerToTargetMessage)
        {
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
        }
    }
}
