using DDDNetCore.Domain.Shared;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Players
{
    public class Connection : Entity<ConnectionId>
    {
        //To avoid circular problems that are not ok according DDD principles we avoid having type user and instead use their id
        public PlayerId Friend { get; private set; }

        public ConnectionStrength ConnectionStrength { get; private set; }

        public ICollection<Tag> Tags { get; private set; }

        public Connection(PlayerId friend)
        {
            Friend = friend;
            Tags = new List<Tag>();
        }

        public void ChangeTags(ICollection<string> tags) 
        {
            ICollection<Tag> finalTags = new List<Tag>();
            foreach (string str in tags)
            {
                finalTags.Add(new Tag(str));
            }
            this.Tags = finalTags;
        }

        public void ChangeConnectionStrength(int connectionStrength ) 
        {
            this.ConnectionStrength = new ConnectionStrength(connectionStrength);
        }


    }
}
