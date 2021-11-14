using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Players
{
    public class Player : Entity<PlayerId>, IAggregateRoot
    {
     
        public PlayerName Name { get;  private set; }
        public PlayerEmail Email { get;  private set; }
        public PlayerPassword Password { get;  private set; }
        public PlayerDateOfBirth DateOfBirth { get;  private set; }
        public PlayerPhoneNumber PhoneNumber { get;  private set; }
        public PlayerEmotionalStatus EmotionalStatus { get;  private set; }
        public PlayerFacebook Facebook { get;  private set; }
        public PlayerLinkedIn LinkedIn { get;  private set; }

        public bool Active{ get;  private set; }

        private Player()
        {
            this.Active = true;
        }

        public Player(string name, string email, string password, string phoneNumber, int year, int month, int day, string emotionalStatus, string facebook, string linkedin)
        {
            this.Id = new PlayerId(Guid.NewGuid());
            this.Name = new PlayerName(name);
            this.Email = new PlayerEmail(email);
            this.Password = new PlayerPassword(password);
            this.PhoneNumber = new PlayerPhoneNumber(phoneNumber);
            this.DateOfBirth = new PlayerDateOfBirth(year, month, day);
            _ = Enum.TryParse(emotionalStatus, out OOC status);
            EmotionalStatus = new PlayerEmotionalStatus(status);
            this.Facebook = new PlayerFacebook(facebook);
            this.LinkedIn = new PlayerLinkedIn(linkedin);
            this.Active = true;
        }

        public void ChangeName(string name)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the name to an inactive Player.");
            this.Name = new PlayerName(name);
        }

        
        public void ChangeEmail(string email)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the email address to an inactive Player.");
            this.Email = new PlayerEmail(email);
        }

        public void ChangePassword(string password)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the password to an inactive Player.");
            this.Password = new PlayerPassword(password);
        }
        
        public void ChangePhoneNumber(string phoneNumber)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the phone number to an inactive Player.");
            this.PhoneNumber = new PlayerPhoneNumber(phoneNumber);
        }
        
        public void ChangeDateOfBirth(int year, int month, int day)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the date of birth to an inactive Player.");
            this.DateOfBirth = new PlayerDateOfBirth(year, month, day);
        }

        public void ChangeEmotionalStatus(string emotionalStatus)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the emotional status to an inactive Player.");
            _ = Enum.TryParse(emotionalStatus, out OOC status);
            EmotionalStatus = new PlayerEmotionalStatus(status);
        }

        public void ChangeFacebook(string facebook)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the facebook Player to an inactive Player.");
            this.Facebook = new PlayerFacebook(facebook);
        }
        
        public void ChangeLinkedIn(string linkedIn)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the linkedIn Player to an inactive Player.");
            this.LinkedIn = new PlayerLinkedIn(linkedIn);
        }        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}