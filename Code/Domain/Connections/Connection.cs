using DDDNetCore.Domain.Shared;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class Connection : Entity<ConnectionId>, IAggregateRoot
    {
        public PlayerId Player { get; private set; }
        
        public PlayerId Friend { get; private set; }

        public ConnectionStrength ConnectionStrength { get; private set; }

        public ICollection<Tag> Tags { get; private set; }

        public bool Active { get; private set; }

        private Connection()
        {
            Active = true;
        }

        public Connection(string player, string friend)
        {
            Id = new ConnectionId(Guid.NewGuid());
            Player = new PlayerId(player);
            Friend = new PlayerId(friend);
            ConnectionStrength = new ConnectionStrength(0);
            Tags = new List<Tag>();
            Active = true;
        }

        public void ChangePlayer(string player) 
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the player of an inactive Connection!");
            }
            Player = new PlayerId(player);
        }

        public void ChangeFriend(string friend)
        { 
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the friend of an inactive Connection!");
            }
            Friend = new PlayerId(friend);
        }

        public void ChangeTags(ICollection<string> tags) 
        {
            if (!Active)
                throw new BusinessRuleValidationException("It is not possible to change the tags of an inactive Connection!");
            ICollection<Tag> finalTags = new List<Tag>();
            foreach (string str in tags)
            {
                finalTags.Add(new Tag(str));
            }
            Tags = finalTags;
        }

        public void ChangeConnectionStrength(int connectionStrength ) 
        {
            if (!Active)
                throw new BusinessRuleValidationException("It is not possible to change the strength of an inactive Connection!");
            ConnectionStrength = new ConnectionStrength(connectionStrength);
        }

        public void MarkAsInactive() 
        {
            Active = false;
        }

        public override bool Equals(object obj)
        {
            return obj is Connection connection &&
                   EqualityComparer<PlayerId>.Default.Equals(Player, connection.Player) &&
                   EqualityComparer<PlayerId>.Default.Equals(Friend, connection.Friend);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Player, Friend);
        }
    }
}
