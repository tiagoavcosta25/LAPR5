using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Profile
{
    public class Profile : Entity<ProfileId>, IAggregateRoot
    {
     
        public ProfileDateOfBirth DateOfBirth { get;  private set; }
        public ProfilePhoneNumber PhoneNumber { get;  private set; }
        public ProfileEmail Email { get;  private set; }
        public ProfileName Name { get;  private set; }
        public string Description { get;  private set; }

        public bool Active{ get;  private set; }

        private Profile()
        {
            this.Active = true;
        }

        public Profile(string description)
        {
            this.Id = new ProfileId(Guid.NewGuid());
            this.Description = description;
            this.Active = true;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the description to an inactive Profile.");
            this.Description = description;
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}