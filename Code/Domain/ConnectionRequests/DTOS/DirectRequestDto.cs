using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class DirectRequestDto : ConnectionRequestDto
    {
        public DirectRequestDto(string id, string player, string target, string playerToTargetMessage, string currentStatus, int strength, ICollection<string> tags) : base(id, player, target, playerToTargetMessage, currentStatus, strength, tags)
        {
        }
    }
}
