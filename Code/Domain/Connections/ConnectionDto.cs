using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class ConnectionDto
    {
        public Guid Id { get; set; }

        public PlayerId Player { get; set; }

        public PlayerId Friend { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public ConnectionDto(Guid id, PlayerId player, PlayerId friend)
        {
            Id = id;
            Player = player;
            Friend = friend;
        }

        public ConnectionDto(Guid id, PlayerId player, PlayerId friend, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            Player = player;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
