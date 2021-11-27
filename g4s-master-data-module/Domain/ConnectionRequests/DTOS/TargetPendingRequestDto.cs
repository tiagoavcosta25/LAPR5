using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public abstract class TargetPendingRequestDto
    {
        public string Id { get; set; }
        public PlayerDto Player { get; set; }
        public PlayerDto Target { get; set; }
        public string PlayerToTargetMessage { get; set; }

        protected TargetPendingRequestDto(string id, PlayerDto player, PlayerDto target, string playerToTargetMessage)
        {
            Id = id;
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
        }
    }
}
