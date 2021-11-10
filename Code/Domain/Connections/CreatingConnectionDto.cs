using DDDSample1.Domain.Players;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class CreatingConnectionDto
    {
        public string Player { get; set; }

        public string Friend { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public CreatingConnectionDto(string player, string friend, int connectionStrength, ICollection<string> tags)
        {
            Player = player;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
