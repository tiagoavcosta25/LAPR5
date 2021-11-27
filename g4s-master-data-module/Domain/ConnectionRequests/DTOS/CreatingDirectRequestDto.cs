using System.Collections.Generic;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class CreatingDirectRequestDto
    {
        public string Player { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public int Strength { get; set; }
        public ICollection<string> Tags { get; set; }

        public CreatingDirectRequestDto(string player, string target, string playerToTargetMessage, int strength, ICollection<string> tags)
        {
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            Strength = strength;
            Tags = tags;
        }
    }
}
