using DDDSample1.Domain.Players;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Players
{
    public class CreatingConnectionDto
    {
        public PlayerId Friend { get; private set; }

        public int ConnectionStrength { get; private set; }

        public ICollection<string> Tags { get; private set; }

        public CreatingConnectionDto(PlayerId friend, int connectionStrength, ICollection<string> tags)
        {
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
