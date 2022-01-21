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

        Task<PlayerDto> GetByEmailAsync(string email);

        Task<List<PlayerDto>> GetByNameAsync(string name);

        Task<List<PlayerDto>> GetByPhoneAsync(string phoneNumber);

        Task<List<PlayerDto>> GetByTagAsync(string tag);

        ICollection<string> GetFilters();
       
        Task<List<GetPlayerSuggestionDto>> GetSuggestions(string playerEmail);

        Task<int> getPlayerNumber();

        Task<int> Login(string playerEmail, string playerPassword); 
    }
}