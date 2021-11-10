using DDDSample1.Domain.Players;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class CreatingConnectionDto
    {
        public PlayerId Player { get; private set; }

        public PlayerId Friend { get; private set; }

        public int ConnectionStrength { get; private set; }

        public ICollection<string> Tags { get; private set; }

        public CreatingConnectionDto(PlayerId player, PlayerId friend, int connectionStrength, ICollection<string> tags)
        {
            Player = player;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
