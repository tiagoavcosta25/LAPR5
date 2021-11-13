using DDDNetCore.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public abstract class ConnectionRequestDto
    {
        public string Id { get; set; }
        public string Player { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public string CurrentStatus { get; set; }
        public int Strength { get; set; }
        public ICollection<string> Tags {get; set;}

        public ConnectionRequestDto(string id, string player, string target, string playerToTargetMessage,
            string currentStatus, int strength, ICollection<string> tags)
        {
            Id = id;
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            CurrentStatus = currentStatus;
            Strength = strength;
            Tags = tags;
        }
    }
}
