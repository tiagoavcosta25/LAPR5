using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Players
{
    public class ConnectionDto
    {
        public Guid Id { get; private set; }

        public PlayerId Friend { get; private set; }

        public int ConnectionStrength { get; private set; }

        public ICollection<string> Tags { get; private set; }

        public ConnectionDto(Guid id, PlayerId friend, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
