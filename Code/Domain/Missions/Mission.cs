using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Missions
{
    public class Mission : Entity<MissionId>, IAggregateRoot
    {
        public PlayerId Challenger { get; private set; }

        public PlayerId Objective { get; private set; }

        public MissionDifficulty Difficulty { get; private set; }

        public MissionStatus CurrentStatus { get; private set; }

        public bool Active { get; private set; }

        private Mission() 
        {
            Active = true;
        }

        public Mission(string challenger, string objective, int difficulty, string currentStatus)
        {
            Id = new MissionId(Guid.NewGuid());
            Challenger = new PlayerId(challenger);
            Objective = new PlayerId(objective);
            Difficulty = new MissionDifficulty(difficulty);
            _ = Enum.TryParse(currentStatus, out MissionStatusEnum status);
            CurrentStatus = new MissionStatus(status);
            this.Active = true;
        }

        public void ChangeChallenger(string challenger) 
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the challenger of an inactive Mission!");
            }
            Challenger = new PlayerId(challenger);
        }

        public void ChangeObjective(string objective)
        {
            if (!Active)
            { 
                throw new BusinessRuleValidationException("It is not possible to change the objective of an inactive Mission!");
            }
            Objective = new PlayerId(objective);
        }

        public void ChangeDifficulty(int difficulty)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the difficulty of an inactive Mission!");
            }
            Difficulty = new MissionDifficulty(difficulty);
        }

        public void ChangeCurrentStatus(string currentStatus)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the current status of an inactive Mission!");
            }
            _ = Enum.TryParse(currentStatus, out MissionStatusEnum status);
            CurrentStatus = new MissionStatus(status);
        }

        public void MarkAsInactive()
        {
            Active = false;
        }

        public override bool Equals(object obj)
        {
            return obj is Mission mission &&
                   EqualityComparer<PlayerId>.Default.Equals(Challenger, mission.Challenger) &&
                   EqualityComparer<PlayerId>.Default.Equals(Objective, mission.Objective);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Challenger, Objective);
        }
    }
}
