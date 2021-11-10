using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class ConnectionDto
    {
        public Guid Id { get; private set; }

        public PlayerId Player { get; private set; }

        public PlayerId Friend { get; private set; }

        public int ConnectionStrength { get; private set; }

        public ICollection<string> Tags { get; private set; }

        public ConnectionDto(Guid id, PlayerId player, PlayerId friend, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            Player =
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
