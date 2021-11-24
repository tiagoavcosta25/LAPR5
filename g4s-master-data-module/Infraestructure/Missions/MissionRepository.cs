using DDDNetCore.Domain.Missions;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Infraestructure.Missions
{
    public class MissionRepository : BaseRepository<Mission, MissionId>, IMissionRepository
    {
        private readonly DbSet<Mission> _dbmission;

        public MissionRepository(DDDSample1DbContext context) : base(context.Missions)
        {
            _dbmission = context.Missions;
        }
    }
}
