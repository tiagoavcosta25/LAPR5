using DDDSample1.Domain.Profiles;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Profiles
{
    public class CreatingConnectionDto
    {
        public ProfileId Friend { get; private set; }

        public int ConnectionStrength { get; private set; }

        public ICollection<string> Tags { get; private set; }

        public CreatingConnectionDto(ProfileId friend, int connectionStrength, ICollection<string> tags)
        {
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
