using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDSample1.Domain.Players
{
    public interface IPlayerService
    {
        Task<List<PlayerDto>> GetAllAsync();

        Task<PlayerDto> GetByIdAsync(PlayerId id);

        Task<PlayerDto> AddAsync(CreatingPlayerDto dto);

        Task<PlayerDto> UpdateAsync(UpdatePlayerDto dto);

        Task<PlayerDto> InactivateAsync(PlayerId id);

        Task<PlayerDto> DeleteAsync(PlayerId id);

        Task<ChangeEmotionalStatusDto> ChangeEmotionalStatusAsync(ChangeEmotionalStatusDto dto);

        Task<GetPlayerDto> GetByEmailAsync(string email);

        Task<List<GetPlayerDto>> GetByNameAsync(string name);

        Task<List<GetPlayerDto>> GetByPhoneAsync(string phoneNumber);

        Task<List<GetPlayerDto>> GetByTagAsync(string tag);
    }
}