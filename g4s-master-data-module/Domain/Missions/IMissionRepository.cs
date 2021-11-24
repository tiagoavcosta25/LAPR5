using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Missions
{
    public interface IMissionRepository : IRepository<Mission, MissionId>
    {
    }
}
