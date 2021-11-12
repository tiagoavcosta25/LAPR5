using DDDNetCore.Domain.Missions.DTOS;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Missions
{
    public class MissionService
    {
        private readonly IUnitOfWork _unitOfWork;

        private readonly IMissionRepository _repo;

        private readonly IPlayerRepository _repoPl;

        public MissionService(IUnitOfWork unitOfWork, IMissionRepository repo, IPlayerRepository repoPl)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repoPl = repoPl;
        }

        public async Task<List<MissionDto>> GetAllAsync()
        {
            var list = await _repo.GetAllAsync();

            List<MissionDto> listDto = list.ConvertAll<MissionDto>(mis =>
                new MissionDto(mis.Id.AsString(), mis.Challenger.AsString(), mis.Objective.AsString(), mis.Difficulty.Difficulty, mis.CurrentStatus.CurrentStatus.ToString()));

            return listDto;
        }

        public async Task<MissionDto> GetByIdAsync(MissionId id)
        {
            var mis = await _repo.GetByIdAsync(id);

            if (mis == null)
                return null;
            
            return new MissionDto(mis.Id.AsString(), mis.Challenger.AsString(), mis.Objective.AsString(), mis.Difficulty.Difficulty, mis.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<MissionDto> AddAsync(CreatingMissionDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Challenger));
            await checkPlayerIdAsync(new PlayerId(dto.Objective));
            var mis = new Mission(dto.Challenger.ToString(), dto.Objective.ToString(), dto.Difficulty, dto.CurrentStatus);

            await _repo.AddAsync(mis);

            await _unitOfWork.CommitAsync();

            return new MissionDto(mis.Id.AsString(), mis.Challenger.AsString(), mis.Objective.AsString(), mis.Difficulty.Difficulty, mis.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<MissionDto> UpdateAsync(MissionDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Challenger));
            await checkPlayerIdAsync(new PlayerId(dto.Objective));
            var mis = await _repo.GetByIdAsync(new MissionId(dto.Id));

            if (mis == null)
                return null;

            mis.ChangeChallenger(dto.Challenger);
            mis.ChangeObjective(dto.Objective);
            mis.ChangeDifficulty(dto.Difficulty);
            mis.ChangeCurrentStatus(dto.CurrentStatus);

            await _unitOfWork.CommitAsync();

            return new MissionDto(mis.Id.AsString(), mis.Challenger.AsString(), mis.Objective.AsString(), mis.Difficulty.Difficulty, mis.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<MissionDto> InactivateAsync(MissionId id)
        {
            var mis = await _repo.GetByIdAsync(id);

            if (mis == null)
                return null;

            mis.MarkAsInactive();

            await _unitOfWork.CommitAsync();

            return new MissionDto(mis.Id.AsString(), mis.Challenger.AsString(), mis.Objective.AsString(), mis.Difficulty.Difficulty, mis.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<MissionDto> DeleteAsync(MissionId id)
        {
            var mis = await _repo.GetByIdAsync(id);

            if (mis == null)
                return null;

            _repo.Remove(mis);
            await _unitOfWork.CommitAsync();

            return new MissionDto(mis.Id.AsString(), mis.Challenger.AsString(), mis.Objective.AsString(), mis.Difficulty.Difficulty, mis.CurrentStatus.CurrentStatus.ToString());
        }

        private async Task checkPlayerIdAsync(PlayerId playerId)
        {
            var pl = await _repoPl.GetByIdAsync(playerId);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Challenger or Objective Id.");
        }

        private async Task checkPlayerEmailAsync(string playerEmail)
        {
            var pl = await _repoPl.GetByEmailAsync(playerEmail);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Challenger or Objective Email.");
        }


        // CRUD OVER //
    }
}
