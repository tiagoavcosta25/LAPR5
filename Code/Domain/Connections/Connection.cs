using DDDNetCore.Domain.Shared;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class Connection : Entity<ConnectionId>, IAggregateRoot
    {
        //To avoid circular problems that are not ok according DDD principles we avoid having type user and instead use their id
        public PlayerId Player { get; private set; }
        
        public PlayerId Friend { get; private set; }

        public ConnectionStrength ConnectionStrength { get; private set; }

        public ICollection<Tag> Tags { get; private set; }

        public Connection(PlayerId player, PlayerId friend)
        {
            Player = player;
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
            Tags = finalTags;
        }

        public void ChangeConnectionStrength(int connectionStrength ) 
        {
            ConnectionStrength = new ConnectionStrength(connectionStrength);
        }


    }
}
