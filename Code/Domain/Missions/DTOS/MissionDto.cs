namespace DDDNetCore.Domain.Missions.DTOS
{
    public class MissionDto
    {
        public string Id { get; set; }

        public string Challenger { get; set; }

        public string Objective { get; set; }

        public int Difficulty { get; set; }

        public string CurrentStatus { get; set; }

        public MissionDto(string id, string challenger, string objective, int difficulty, string currentStatus) 
        {
            Id = id;
            Challenger = challenger;
            Objective = objective;
            Difficulty = difficulty;
            CurrentStatus = currentStatus;
        }
    }
}
