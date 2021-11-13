using DDDSample1.Domain.Shared;
using System;
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Domain.Missions
{
    public class MissionDifficulty : IValueObject
    {
        [Required]
        public int Difficulty { get; private set; }

        public MissionDifficulty(int difficulty)
        {
            Difficulty = difficulty;
        }

        public void ChangeDifficulty(int difficulty)
        {
            Difficulty = difficulty;
        }

        public override bool Equals(object obj)
        {
            return obj is MissionDifficulty difficulty &&
                   Difficulty == difficulty.Difficulty;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Difficulty);
        }
    }
}
