using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Profiles
{
    public class Profile : Entity<ProfileId>, IAggregateRoot
    {
     
        public ProfileDateOfBirth DateOfBirth { get;  private set; }
        public ProfilePhoneNumber PhoneNumber { get;  private set; }
        public ProfileEmail Email { get;  private set; }
        public ProfileName Name { get;  private set; }

        public bool Active{ get;  private set; }

        private Profile()
        {
            this.Active = true;
        }

        public Profile(string name, string email, double phoneNumber, int year, int month, int day)
        {
            this.Id = new ProfileId(Guid.NewGuid());
            this.Name = new ProfileName(name);
            this.Email = new ProfileEmail(email);
            this.PhoneNumber = new ProfilePhoneNumber(phoneNumber);
            this.DateOfBirth = new ProfileDateOfBirth(year, month, day);
            this.Active = true;
        }

        public void ChangeName(string name)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the name to an inactive Profile.");
            this.Name = new ProfileName(name);
        }

        
        public void ChangeEmail(string email)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the email address to an inactive Profile.");
            this.Email = new ProfileEmail(email);
        }
        
        public void ChangePhoneNumber(double phoneNumber)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the phone number to an inactive Profile.");
            this.PhoneNumber = new ProfilePhoneNumber(phoneNumber);
        }
        
        public void ChangeDateOfBirth(int year, int month, int day)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the date of birth to an inactive Profile.");
            this.DateOfBirth = new ProfileDateOfBirth(year, month, day);
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}