namespace DDDNetCore.Domain.Missions.DTOS
{
    public class CreatingMissionDto
    {
        public string Challenger { get; set; }

        public string Objective { get; set; }

        public int Difficulty { get; set; }

        public string CurrentStatus { get; set; }

        public CreatingMissionDto(string challenger, string objective, int difficulty, string currentStatus)
        {
            Challenger = challenger;
            Objective = objective;
            Difficulty = difficulty;
            CurrentStatus = currentStatus;
        }
    }
}
