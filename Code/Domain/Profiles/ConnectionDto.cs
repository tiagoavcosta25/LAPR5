using DDDSample1.Domain.Profiles;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Profiles
{
    public class ConnectionDto
    {
        public Guid Id { get; private set; }

        public ProfileId Friend { get; private set; }

        public int ConnectionStrength { get; private set; }

        public ICollection<string> Tags { get; private set; }

        public ConnectionDto(Guid id, ProfileId friend, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
