using DDDNetCore.Domain.Shared;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public class IntroductionRequest : ConnectionRequest
    {
        [Required]
        [MaxLength(70)]
        public PlayerId MiddleMan { get; private set; }
        [Required]
        [MaxLength(1000)]
        public Message PlayerToMiddleManMessage { get; private set; }
        [Required]
        [MaxLength(1000)]
        public Message MiddleManToTargetMessage { get; private set; }

        public IntroductionRequest()
        { 
        }

        public IntroductionRequest(string player, string target, string playerToTargetMessage, string currentStatus,
            string middleMan, string playerToMiddleManMessage, string middleManToTargetMessage, int strength, ICollection<string> tags) 
            : base(player, target, playerToTargetMessage, currentStatus, strength, tags)
        {
            MiddleMan = new PlayerId(middleMan);
            PlayerToMiddleManMessage = new Message(playerToMiddleManMessage);
            MiddleManToTargetMessage = new Message(middleManToTargetMessage);
        }

        public void ChangeMiddleMan(string middleMan)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the middle man of an inactive ConnectionRequest!");
            }
            MiddleMan = new PlayerId(middleMan);
        }

        public void ChangePlayerToMiddleManMessage(string playerToMiddleManMessage)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the player to middle man message of an inactive ConnectionRequest!");
            }
            PlayerToMiddleManMessage = new Message(playerToMiddleManMessage);
        }

        public void ChangeMiddleManToTargetMessage(string middleManToTargetMessage)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the middle man to target message of an inactive ConnectionRequest!");
            }
            MiddleManToTargetMessage = new Message(middleManToTargetMessage);
        }
    }
}
