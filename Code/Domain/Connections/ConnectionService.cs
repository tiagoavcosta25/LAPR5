using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Domain.Connections
{
    public class ConnectionService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IConnectionRepository _repo;

        public ConnectionService(IUnitOfWork unitOfWork, IConnectionRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }


    }
}
